################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis"
setwd(project.dir)
################################################################################
################################################################################
## read tx
psvc.tx <- read_rds("data/derivatives/PSVC-clean-transcription.rds")
psvc.tx.split <- psvc.tx %>% mutate(response=str_split(response," ")) %>%
  unnest(response)
################################################################################
################################################################################
## performance stats
psvc.summ <- psvc.tx %>% group_by(te_id, prompt) %>%
  summarise(count = n())

kableExtra::kable(psvc.summ %>% pivot_wider(names_from = prompt, values_from = count, id_cols = te_id), 
                  format="html") %>%
  kableExtra::kable_styling(full_width = T, protect_latex = T)

psvc.summ %>% 
  ggplot(aes(count)) + geom_histogram() +
  geom_vline(xintercept = c(5,10), linetype=2,color="red") +
  facet_wrap(~prompt, scales = "free") +
  bw.theme
ggsave2("figs/PSVC/distributions/word-count-histogram-per-task.png",10,8)

################################################################################
################################################################################
## extract some basic word features using lingmatch
lingmatch.feat <- cbind(response = unique(psvc.tx.split$response),
                        lingmatch::lma_meta(unique(psvc.tx.split$response)),
                        lingmatch::lma_termcat(unique(psvc.tx.split$response))) %>%
  mutate_at(.vars = vars(c(25:33)), .funs = function(x) ifelse(x>0,1,0)) %>%
  mutate(sentence_response = ifelse(words>1,1,0)) %>%
  select(response, characters_per_word, syllables_per_word, sentence_response,
         reading_grade, c(25:33))
# get averaged values per prompt
psvc.lingmatch.feat <- full_join(left_join(psvc.tx.split %>% select(te_id, prompt, response),
                                           lingmatch.feat) %>% select(-response) %>%
                                   group_by(te_id, prompt) %>%
                                   summarise_at(.vars = vars(c(characters_per_word, syllables_per_word,reading_grade)),
                                                .funs = function(x) mean(x, na.rm = T)),
                                 left_join(psvc.tx.split %>% select(te_id, prompt, response),
                                           lingmatch.feat) %>% select(-response) %>%
                                   group_by(te_id, prompt) %>%
                                   summarise_at(.vars = vars(c(colnames(lingmatch.feat)[c(4,6:14)])),
                                                .funs = function(x) sum(x, na.rm = T)))
write_rds(psvc.lingmatch.feat, "data/derivatives/tmp-use/PSVC-lingmatch-features-per-prompt.rds", compress = "gz")


## extract sentiment and word category
sent.feat <- cbind(response = unique(psvc.tx.split$response),
                   syuzhet::get_nrc_sentiment(unique(psvc.tx.split$response)),
                   sentimentr::profanity(unique(psvc.tx.split$response))%>%select(count_profanity=profanity_count)) %>% 
  mutate_at(.vars = vars(-c(response)), .funs = function(x) as.numeric(x>0))

psvc.sent.feat <- left_join(psvc.tx.split %>% select(te_id, prompt, response),
                            sent.feat) %>% select(-response) %>%
  group_by(te_id, prompt) %>%
  summarise_all(.funs = function(x) sum(x, na.rm = T)) %>%
  rename_at(.vars = vars(-c(te_id, prompt)), .funs = function(x) paste0("count_",x))
write_rds(psvc.sent.feat, "data/derivatives/tmp-use/PSVC-sentiment-features-per-prompt.rds", compress = "gz")

################################################################################
################################################################################
## Age of Acquisition (AoA)
aoa.ref <- readxl::read_xlsx("../../../data/corpus/Age-of-Acquisition-AoA/AoA_51715_words.xlsx") %>%
  select(Word, `Alternative.spelling`, AoA_Kup_lem) %>%
  pivot_longer(cols = c(1,2), values_to = "word") %>%
  mutate(word = tolower(word)) %>%
  mutate(AoA_Kup_lem = as.numeric(AoA_Kup_lem)) %>%
  distinct(word, .keep_all = T)
table(unique(psvc.tx.split$response) %in% aoa.ref$word)

psvc.tx.split %>% distinct(te_id, response) %>% 
  left_join(aoa.ref %>% select(response =word, AoA_Kup_lem) %>% distinct(response, .keep_all=T)) %>% 
  group_by(te_id) %>%
  summarise(mean_AoA = mean(AoA_Kup_lem,na.rm = T),
            highest_AoA = max(AoA_Kup_lem, na.rm = T),
            std_AoA = sd(AoA_Kup_lem, na.rm = T)) %>% mutate(prompt="all") %>%
  write_rds("data/derivatives/tmp-use/PSVC-AoA-per-id.rds", compress = "gz")
psvc.tx.split %>% distinct(te_id, prompt, response) %>% 
  left_join(aoa.ref %>% select(response =word, AoA_Kup_lem) %>% distinct(response, .keep_all=T)) %>% 
  group_by(te_id,prompt) %>%
  summarise(mean_AoA = mean(AoA_Kup_lem,na.rm = T),
            highest_AoA = max(AoA_Kup_lem, na.rm = T),
            std_AoA = sd(AoA_Kup_lem, na.rm = T)) %>%
  write_rds("data/derivatives/tmp-use/PSVC-AoA-per-prompt.rds", compress = "gz")

################################################################################
################################################################################
## word familiarity by GPT
gpt.fam <- readxl::read_xlsx("../../../data/corpus/AI-generated-estimates-for-word-familiarity/Full list GPT4 estimates familiarity and Multilex frequencies.xlsx") %>%
  mutate(Word = tolower(Word)) 
table(unique(psvc.tx.split$response) %in% gpt.fam$Word)

psvc.tx.split %>% distinct(te_id, response) %>%
  left_join(gpt.fam %>% mutate(GPT_fam = as.numeric(GPT_Fam_dominant)) %>%
              select(response =Word, GPT_fam) %>% distinct(response, .keep_all=T)) %>% 
  group_by(te_id) %>%
  summarise(min_GPT_fam = min(GPT_fam, na.rm=T),
            mean_GPT_fam = mean(GPT_fam,na.rm = T),
            std_GPT_fam = sd(GPT_fam, na.rm = T)) %>% mutate(prompt="all") %>%
  write_rds("data/derivatives/tmp-use/PSVC-GPT-familiarity-per-id.rds", compress = "gz")

psvc.tx.split %>% distinct(te_id, prompt, response) %>%
  left_join(gpt.fam %>% mutate(GPT_fam = as.numeric(GPT_Fam_dominant)) %>%
              select(response =Word, GPT_fam) %>% distinct(response, .keep_all=T)) %>% 
  group_by(te_id,prompt) %>%
  summarise(min_GPT_fam = min(GPT_fam, na.rm=T),
            mean_GPT_fam = mean(GPT_fam,na.rm = T),
            std_GPT_fam = sd(GPT_fam, na.rm = T)) %>%
  write_rds("data/derivatives/tmp-use/PSVC-GPT-familiarity-per-prompt.rds", compress = "gz")

################################################################################
################################################################################
## MRC psycholinguistic features
mrc.feat <- read_csv("../../../data/corpus/MRC-psycholinguistic-database/mrc_psycholinguistic_database.csv") %>%
  mutate(Word = tolower(Word))
colnames(mrc.feat)
table(unique(psvc.tx.split$response) %in% mrc.feat$Word)

psvc.tx.split %>% distinct(te_id, response) %>%
  left_join(mrc.feat %>% 
              select(response=Word, number_of_phonemes = `Number of Phonemes`,imageability=Imageability) %>% 
              distinct(response, .keep_all=T)) %>%
  group_by(te_id) %>%
  summarise(number_of_phonemes = mean(number_of_phonemes,na.rm = T),
            imageability = mean(imageability, na.rm = T)) %>% mutate(prompt ="all") %>%
  write_rds("data/derivatives/tmp-use/PSVC-MRC-features-per-id.rds", compress = "gz")

psvc.tx.split %>% distinct(te_id, prompt, response) %>%
  left_join(mrc.feat %>% 
              select(response=Word, number_of_phonemes = `Number of Phonemes`,imageability=Imageability) %>% 
              distinct(response, .keep_all=T)) %>%
  group_by(te_id,prompt) %>%
  summarise(number_of_phonemes = mean(number_of_phonemes,na.rm = T),
            imageability = mean(imageability, na.rm = T)) %>% 
  write_rds("data/derivatives/tmp-use/PSVC-MRC-features-per-prompt.rds", compress = "gz")

################################################################################
################################################################################
## calc concreteness score
conc.ref <- readxl::read_xlsx("../../../data/corpus/Brysbaert-et-al-concreteness-ratings.xlsx") %>%
  select(response = Word, concreteness = `Conc.M`) %>%
  mutate(response = tolower(response))
table(unique(psvc.tx.split$response) %in% conc.ref$response)

psvc.tx.split %>% distinct(te_id, response) %>%
  left_join(conc.ref %>% distinct(response, .keep_all=T)) %>%
  group_by(te_id) %>%
  summarise(concreteness = mean(concreteness,na.rm = T)) %>% mutate(prompt="all") %>%
  write_rds("data/derivatives/tmp-use/PSVC-concreteness-per-id.rds")

psvc.tx.split %>% distinct(te_id, prompt,response) %>%
  left_join(conc.ref %>% distinct(response, .keep_all=T)) %>%
  group_by(te_id,prompt) %>%
  summarise(concreteness = mean(concreteness,na.rm = T)) %>% 
  write_rds("data/derivatives/tmp-use/PSVC-concreteness-per-prompt.rds")

################################################################################
################################################################################
## unique word count
psvc.tx %>% distinct(te_id, response) %>%
  group_by(te_id) %>% summarise(count_unique_word_all = n()) %>%
  write_rds("data/derivatives/tmp-use/PSVC-unique-word-count-per-id.rds")
psvc.tx %>% distinct(te_id, prompt, response) %>%
  group_by(te_id, prompt) %>% summarise(count_unique_word_prompt = n()) %>%
  write_rds("data/derivatives/tmp-use/PSVC-unique-word-count-per-prompt.rds")

################################################################################
################################################################################
## word productivity
psvc.count <- read_rds("data/derivatives/tmp-use/PSVC-unique-word-count-per-prompt.rds")
psvc.prod <- psvc.count %>% 
  pivot_wider(names_from = prompt, values_from = count_unique_word_prompt, id_cols = te_id, values_fill = 0) %>%
  rowwise() %>%
  mutate(COWAT_productivity = summary(lm(c_across(unique(psvc.tx$prompt)[21:25]) ~ c(1:5)))$coef[2,1],
         WAT_productivity = summary(lm(c_across(unique(psvc.tx$prompt)[1:20]) ~ c(1:20)))$coef[2,1]) %>%
  select(te_id, COWAT_productivity, WAT_productivity)
psvc.prod %>% write_rds("data/derivatives/tmp-use/PSVC-word-productivity-per-task.rds")

################################################################################
################################################################################
################################################################################
