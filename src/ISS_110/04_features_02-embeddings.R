################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
source("/wdata/msmuhammad/git-repo/ISS_110_docker/scripts/00_initialize.R")
source("/wdata/msmuhammad/git-repo/ISS_110_docker/scripts/04_feature_extraction.R")
source("/wdata/msmuhammad/git-repo/ISS_110_docker/scripts/utils/calc_insight_patterns.R")
library(logging);library(logger)
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis")
setwd(project.dir)
################################################################################
################################################################################
pload("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")

## read tx
iss.tx <- read_rds("data/derivatives/ISS-clean-transcription.rds")%>%rename(participant_id=te_id)%>%
  left_join(ann %>% mutate(prompt=tolower(prompt))) %>%
  mutate(poss_start = start-seconds)

################################################################################
################################################################################
################################################################################
## env prep
embeddings.0 <- list(semantic = rbind(read_rds("/wdata/msmuhammad/git-repo/ISS_110_docker/reference_data/embeddings/semantic_common_50k.rds"),
                                      read_csv("data/tmp/sem_out.csv") %>% 
                                        rename_at(.vars = vars(-c(word)), .funs = function(x)paste0("X",x))),
                     phonetic = rbind(read_rds("/wdata/msmuhammad/git-repo/ISS_110_docker/reference_data/embeddings/phonetic_common_50k.rds"),
                                      read_csv("data/tmp/phon_out.csv") %>% rename(text=word) %>%
                                        rename_at(.vars = vars(-text),.funs = function(x) paste0("Dim",as.numeric(x)+1))))
archetypes <- list(semantic = read_rds("/wdata/msmuhammad/git-repo/ISS_110_docker/reference_data/archetypes/semantic_common_50k.rds"),
                   phonettic = read_rds("/wdata/msmuhammad/git-repo/ISS_110_docker/reference_data/archetypes/phonetic_common_50k.rds"))
reference_data <- load_reference_data("/wdata/msmuhammad/git-repo/ISS_110_docker/reference_data")
################################################################################
################################################################################
config <- yaml::read_yaml("/wdata/msmuhammad/git-repo/ISS_110_docker/config/task_template.yaml")

# 1. GENERAL FEATURES
general_features <- extract_general_features(iss.tx, reference_data)

# 2. TEMPORAL FEATURES
temporal_features <- extract_temporal_features(iss.tx%>%dplyr::filter(!is.na(poss_start)&poss_start>0)%>%
                                                 mutate(start=poss_start,task_type="WORD_ASSOC"), reference_data)

# 3. SEMANTIC FEATURES
semantic_features <- extract_embedding_features(
  iss.tx %>% drop_na(start), embeddings.0$semantic %>% dplyr::rename(text=word), archetypes$semantic,
  prefix = "sem", embedding_type = "semantic", config
)

# Anchor similarity
anchors_features <- calculate_anchor_set_similarity(
  iss.tx, 
  embeddings.0$semantic %>% as.data.frame() %>% dplyr::rename(text=word) %>% column_to_rownames("text"),
  prefix = "sem", config
)

# 4. PHONETIC EMBEDDING FEATURES
phonetic_features <- extract_embedding_features(
  iss.tx %>% drop_na(start), embeddings.0$phonetic %>% as.data.frame() %>% distinct(text,.keep_all = T), archetypes$phonetic,
  prefix = "pho", embedding_type = "phonetic", config
)

################################################################################
################################################################################
keep <- list(per_prompt = list(general_features = general_features$per_prompt %>% dplyr::filter(participant_id != "2E_097"),
                               temporal_features = temporal_features$per_prompt %>% dplyr::filter(participant_id != "2E_097"),
                               semantic_features = semantic_features$per_prompt %>% dplyr::filter(participant_id != "2E_097"),
                               phonetic_features = phonetic_features$per_prompt %>% dplyr::filter(participant_id != "2E_097")),
             per_type=list(general_features = general_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                             left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id,type) %>%
                             summarise(across(where(is.numeric), mean, na.rm = TRUE)),
                           temporal_features = temporal_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                             left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id,type) %>%
                             summarise(across(where(is.numeric), mean, na.rm = TRUE)),
                           semantic_features = semantic_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                             left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id,type) %>%
                             summarise(across(where(is.numeric), mean, na.rm = TRUE)),
                           phonetic_features = phonetic_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                             left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id,type) %>%
                             summarise(across(where(is.numeric), mean, na.rm = TRUE))),
             per_participant = list(general_features = general_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                                      left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id) %>%
                                      summarise(across(where(is.numeric), mean, na.rm = TRUE)),
                                    temporal_features = temporal_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                                      left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id) %>%
                                      summarise(across(where(is.numeric), mean, na.rm = TRUE)),
                                    semantic_features = semantic_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                                      left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id) %>%
                                      summarise(across(where(is.numeric), mean, na.rm = TRUE)),
                                    phonetic_features = phonetic_features$per_prompt %>% dplyr::filter(participant_id != "2E_097") %>% 
                                      left_join(iss.tx %>% distinct(prompt,type)) %>% group_by(participant_id) %>%
                                      summarise(across(where(is.numeric), mean, na.rm = TRUE))))
write_rds(keep,"data/derivatives/ISS_app--based-features.rds",compress = "gz")

################################################################################
################################################################################