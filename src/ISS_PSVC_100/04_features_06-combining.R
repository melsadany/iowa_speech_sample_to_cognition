################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis")
setwd(project.dir)
################################################################################
################################################################################
################################################################################
psvc.tx <- read_rds("data/derivatives/PSVC-clean-transcription.rds")
## read general features
nrc.sent <- read_rds("data/derivatives/tmp-use/PSVC-sentiment-features-per-prompt.rds")%>%rename(count_profanity=count_count_profanity)
lingmatch <- read_rds("data/derivatives/tmp-use/PSVC-lingmatch-features-per-prompt.rds")
ums <- read_rds("data/derivatives/tmp-use/PSVC-ums-count.rds")
word.count <- rbind(read_rds("data/derivatives/tmp-use/PSVC-unique-word-count-per-prompt.rds") %>% rename(count=3),
                    read_rds("data/derivatives/tmp-use/PSVC-unique-word-count-per-id.rds") %>% mutate(prompt="all") %>% rename(count=2))
thinking.time <- read_rds("data/derivatives/tmp-use/PSVC-thinking-time-per-prompt.rds")
conc <- rbind(read_rds("data/derivatives/tmp-use/PSVC-concreteness-per-id.rds"),
              read_rds("data/derivatives/tmp-use/PSVC-concreteness-per-prompt.rds"))
bins.stats <- read_rds("data/derivatives/tmp-use/PSVC-bins-statistics-per-prompt.rds")
burst <- read_rds("data/derivatives/tmp-use/PSVC-burst-per-prompt.rds")
AoA <- rbind(read_rds("data/derivatives/tmp-use/PSVC-AoA-per-id.rds"),
             read_rds("data/derivatives/tmp-use/PSVC-AoA-per-prompt.rds"))
GPT.fam <- rbind(read_rds("data/derivatives/tmp-use/PSVC-GPT-familiarity-per-id.rds"),
                 read_rds("data/derivatives/tmp-use/PSVC-GPT-familiarity-per-prompt.rds"))
mrc.ling <- rbind(read_rds("data/derivatives/tmp-use/PSVC-MRC-features-per-id.rds"),
                  read_rds("data/derivatives/tmp-use/PSVC-MRC-features-per-prompt.rds"))
product <- read_rds("data/derivatives/tmp-use/PSVC-word-productivity-per-task.rds")
onset <- read_rds("data/derivatives/tmp-use/PSVC-onset-per-prompt.rds")
comments <- read_rds("data/derivatives/tmp-use/PSVC-comments-count.rds")
rep.prompt <- read_rds("data/derivatives/tmp-use/PSVC-repeated-prompt-count.rds")
rep.word.prompt <- read_rds("data/derivatives/tmp-use/PSVC-repeated-words-per-prompt-count.rds")
overtime <- read_rds("data/derivatives/tmp-use/PSVC-last-response-gap-per-prompt.rds")


sem.pairs.sim <- read_rds("data/derivatives/tmp-use/PSVC-pairs-similarity-per-prompt.rds")
pwe.pairs.sim <- read_rds("data/derivatives/tmp-use/PSVC-PWE-pairs-similarity-per-prompt.rds")
sem.euc <- read_rds("data/derivatives/tmp-use/PSVC-path-euclidean-distance-per-prompt.rds")
pwe.euc <- read_rds("data/derivatives/tmp-use/PSVC-PWE-path-euclidean-distance-per-prompt.rds")
sem.div <- read_rds("data/derivatives/tmp-use/PSVC-divergence-prompt-starting-per-prompt.rds")[,-c(3,4)]
pwe.div <- read_rds("data/derivatives/tmp-use/PSVC-PWE-divergence-prompt-starting-per-prompt.rds")[,-c(3,4)]
sem.vol <- read_rds("data/derivatives/tmp-use/PSVC-vocabulary-volume-per-prompt.rds")
pwe.vol <- read_rds("data/derivatives/tmp-use/PSVC-PWE-vocabulary-volume-per-prompt.rds")
sem.arch <- read_rds("data/derivatives/tmp-use/PSVC-communities-meta-per-prompt.rds")
pwe.arch <- read_rds("data/derivatives/tmp-use/PSVC-PWE-communities-meta-per-prompt.rds")
sem.arch.area <- read_rds("data/derivatives/tmp-use/PSVC-archetypal-area-per-prompt.rds")
pwe.arch.area <- read_rds("data/derivatives/tmp-use/PSVC-PWE-archetypal-area-per-prompt.rds")
sem.arch.rich <- read_rds("data/derivatives/tmp-use/PSVC-archetypal-richness-per-prompt.rds")
pwe.arch.rich <- read_rds("data/derivatives/tmp-use/PSVC-PWE-archetypal-richness-per-prompt.rds")
sem.norms <- read_rds("data/derivatives/tmp-use/PSVC-norms-per-prompt.rds")
pwe.norms <- read_rds("data/derivatives/tmp-use/PSVC-PWE-norms-per-prompt.rds")
sem.prompt.sim.traj <- read_rds("data/derivatives/tmp-use/PSVC-prompt-similarity-trajectory-per-prompt.rds")
pwe.prompt.sim.traj <- read_rds("data/derivatives/tmp-use/PSVC-PWE-prompt-similarity-trajectory-per-prompt.rds")

sem.sim.to.anchors <- read_rds("data/derivatives/tmp-use/PSVC-similarity-to-anchor-sets-per-prompt.rds")

insight.pattern <- read_rds("data/derivatives/tmp-use/PSVC-insight-patterns-per-prompt.rds")
# previously done features
acoustics <- read_csv("data/derivatives/surfboard_audio_features_ALL.csv")
ran.duration <- read_csv("data/derivatives/RAN-durations.csv")%>%pivot_wider(names_from = dim,values_from=duration,names_prefix = "RAN_duration_")
ps_faces <- read_csv("data/derivatives/faces-durations.csv")%>%rename("PS_faces_matching_duration"=2)
################################################################################
################################################################################
################################################################################

per.prompt <- full_join(nrc.sent%>%ungroup,lingmatch%>%ungroup) %>%
  full_join(ums%>%ungroup) %>% full_join(word.count%>%filter(prompt!="all")%>%rename(unique_word_count=3)%>%ungroup)%>%
  full_join(thinking.time%>%ungroup) %>% full_join(bins.stats%>%ungroup) %>% 
  full_join(burst%>%ungroup) %>% full_join(onset%>%ungroup) %>%
  full_join(comments%>%ungroup) %>% full_join(rep.prompt%>%ungroup) %>% 
  full_join(rep.word.prompt%>%ungroup) %>% full_join(overtime%>%ungroup) %>%
  full_join(conc%>%filter(prompt!="all")%>%ungroup)%>%
  full_join(AoA%>%filter(prompt!="all")%>%ungroup)%>%
  full_join(GPT.fam%>%filter(prompt!="all")%>%ungroup)%>%
  full_join(mrc.ling%>%filter(prompt!="all")%>%ungroup)%>%
  full_join(sem.pairs.sim%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.pairs.sim%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.euc%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.euc%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.div%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.div%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.vol%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.vol%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.arch%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.arch%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.arch.area%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.arch.area%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.arch.rich%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.arch.rich%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.norms%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.norms%>%ungroup%>%filter(prompt!="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.prompt.sim.traj%>%ungroup%>%select(1,prompt=2,3)%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(pwe.prompt.sim.traj%>%ungroup%>%select(1,prompt=2,3)%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x)))%>%
  full_join(sem.sim.to.anchors%>%ungroup%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x)))%>%
  full_join(insight.pattern%>%ungroup%>%filter(prompt!="all"))%>%
  mutate(repetitions_errors=rep_prompt+rep_words_per_prompt,
         off_topic_error=comment_count+sentence_response)%>%
  select(-c(halfs_ratio,rep_prompt,rep_words_per_prompt,comment_count,sentence_response,contains("vocabulary_volume")))
  

per.id <- full_join(nrc.sent%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)sum(x,na.rm=T)),
                    lingmatch%>%ungroup%>%select(-prompt,-c(6:15))%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T))) %>%
  full_join(lingmatch%>%ungroup%>%select(-prompt,-c(3:5))%>%group_by(te_id)%>%summarise_all(.funs=function(x)sum(x,na.rm=T))) %>%
  full_join(ums%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)sum(x,na.rm=T))) %>%
  full_join(word.count%>%filter(prompt=="all")%>%select(1,unique_word_count=3)%>%ungroup) %>%
  full_join(thinking.time%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(conc%>%ungroup%>%filter(prompt=="all")%>%select(-prompt))%>%
  full_join(AoA%>%ungroup%>%filter(prompt=="all")%>%select(-prompt))%>%
  full_join(GPT.fam%>%ungroup%>%filter(prompt=="all")%>%select(-prompt))%>%
  full_join(mrc.ling%>%ungroup%>%filter(prompt=="all")%>%select(-prompt))%>%
  full_join(bins.stats%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T))) %>%
  full_join(burst%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T))) %>%
  full_join(product%>%ungroup) %>%
  full_join(onset%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T))) %>%
  full_join(comments%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)sum(x,na.rm=T))) %>%
  full_join(rep.prompt%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)sum(x,na.rm=T))) %>%
  full_join(rep.word.prompt%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)sum(x,na.rm=T)))%>%
  full_join(overtime%>%ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(sem.pairs.sim%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(pwe.pairs.sim%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(sem.euc%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(pwe.euc%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(sem.div%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(pwe.div%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(sem.vol%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(pwe.vol%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(sem.arch%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(pwe.arch%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(sem.arch.area%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(pwe.arch.area%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(sem.arch.rich%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(pwe.arch.rich%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(sem.norms%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(pwe.norms%>%filter(prompt=="all")%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt))%>%
  full_join(sem.prompt.sim.traj%>%select(1,prompt=2,3)%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(pwe.prompt.sim.traj%>%select(1,prompt=2,3)%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("pwe__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(sem.sim.to.anchors%>%rename_at(.vars = vars(-c(te_id,prompt)),.funs=function(x)paste0("sem__",x))%>%
              ungroup%>%select(-prompt)%>%group_by(te_id)%>%summarise_all(.funs=function(x)mean(x,na.rm=T)))%>%
  full_join(insight.pattern%>%filter(prompt=="all")%>%ungroup%>%select(-prompt))%>%
  mutate(repetitions_errors=rep_prompt+rep_words_per_prompt,
         off_topic_error=comment_count+sentence_response)%>%
  select(-c(halfs_ratio,rep_prompt,rep_words_per_prompt,comment_count,sentence_response,contains("vocabulary_volume")))

summary(per.id)
per.id <- per.id %>% mutate(ums_count=ifelse(is.na(ums_count),0,ums_count))
################################################################################
################################################################################
################################################################################
## combining features per task type and cleaning some features
summary(per.prompt)
sum.vars <- c(colnames(nrc.sent)[-c(1,2)],colnames(lingmatch)[-c(1:6)],"ums_count",
              "unique_word_count","off_topic_error","repetitions_errors")
mean.vars <- setdiff(colnames(per.prompt)[-c(1:2)],c("WAT_productivity","COWAT_productivity",sum.vars))
per.task <- inner_join(per.prompt %>% mutate(task = ifelse(nchar(prompt)==1, "COWAT", "WAT")) %>% relocate(task) %>%
             group_by(te_id, task) %>% summarise_at(.vars = vars(all_of(sum.vars)), 
                                                    .funs=function(x)sum(x,na.rm=T)),
           per.prompt %>% mutate(task = ifelse(nchar(prompt)==1, "COWAT", "WAT")) %>% relocate(task) %>%
             group_by(te_id, task) %>%
             summarise_at(.vars = vars(mean.vars), .funs=function(x)mean(x[!is.infinite(x)],na.rm=T))) %>%
  inner_join(product %>% pivot_longer(-1, names_to = "task", values_to = "productivity")%>%
               mutate(task=sub("_.*","",task)))%>%
  mutate(pattern_regularity=ifelse(is.na(pattern_regularity),0,pattern_regularity))
################################################################################
################################################################################
################################################################################
psvc.all <- list("clean_tx" = psvc.tx,
                 "raw_features" = list("NRC"=nrc.sent,"lingmatch",lingmatch,"ums"=ums,"word_count"=word.count,"thinking_time"=thinking.time,
                                       "concreteness"=conc,"bins_stats"=bins.stats,"burst"=burst,"AoA"=AoA,"onset"=onset,"comments"=comments,
                                       "rep_prompt"=rep.prompt,"rep_words_per_prompt"=rep.word.prompt,"overtime"=overtime,
                                       "sem_pairs_sim"=sem.pairs.sim,"pwe_pairs_sim"=pwe.pairs.sim,"sem_euc"=sem.euc,"pwe_euc"=pwe.euc,
                                       "sem_div"=sem.div,"pwe_div"=pwe.div,"sem_vol"=sem.vol,"pwe_vol"=pwe.vol,"sem_arch"=sem.arch,
                                       "pwe_arch"=pwe.arch,"sem_arch_area"=sem.arch.area,"pwe_arch_area"=pwe.arch.area,
                                       "sem_arch_richness"=sem.arch.rich,"pwe_arch_richness"=pwe.arch.rich,
                                       "sem_norms"=sem.norms,"pwe_norms"=pwe.norms,
                                       "sem_sim_to_anchors"=sem.sim.to.anchors,"RAN_acustics"=acoustics,"RAN_duration"=ran.duration,
                                       "PS_faces_duration"=ps_faces,"insight_pattern"=insight.pattern),
                 "prompt_based_features"=per.prompt,
                 "task_based_features"=per.task,
                 "per_participant_features"=per.id)
write_rds(psvc.all,"data/derivatives/PSVC-USE-THIS.rds",compress = "gz")
################################################################################
################################################################################
################################################################################
lang.features <- data.frame(feature = unique(c(colnames(per.prompt)[-c(1:2)],
                                               colnames(per.task)[-c(1:2)],
                                               colnames(per.id)[-1], colnames(ps_faces)[2], colnames(ran.duration)[2:3]))) %>%
  mutate(cat2 = case_when(grepl("sem__",feature)~"semantic",
                          grepl("pwe__",feature)~"phonetic",
                          grepl("duration|onset|_time|last_response",feature)~"timing",
                          grepl("ums_|rep_|comment|sentence_response|off_topic|repetition",feature)~"rule violation",
                          grepl("unique_|GPT_fam",feature)~"general",
                          grepl("halfs|ks_st|Gini_|bins_s|burst_|productiv|insight|pattern",feature)~"trajectory & distribution",
                          feature%in%colnames(nrc.sent)~"emotional valence",
                          feature %in% colnames(lingmatch)|grepl("concre|AoA|imageabi|phonemes",feature) ~ "linguistic features",
                          # (feature%in%c(colnames(nrc.sent),colnames(lingmatch))|
                          #    grepl("concre|AoA|imageabi|phonemes",feature))~"dictionary-based",
                          T~""),
         clean_name = str_replace_all(str_replace_all(feature,"sem__|pwe__|count_|count|_count|PS_",""),"_"," "),
         clean_name = sub("std", "SD", clean_name),clean_name = sub("AoA", "age of acquisition", clean_name),
         clean_name = sub("fam", "familiarity", clean_name),
         clean_name2 = case_when(clean_name=="onset"~"response latency",
                                 grepl("thinking time",clean_name)~ sub("thinking time","inter-word intervals",clean_name),
                                 clean_name=="last response gap"~ "terminal response latency",
                                 grepl("burst",clean_name)~ sub("burst","burstiness",clean_name),
                                 clean_name=="bins slope"~"within-task productivity slope",
                                 grepl("insight pattern",clean_name)~ sub("insight pattern","post-impasse acceleration",clean_name),
                                 clean_name=="productivity"~ "across-prompt productivity slope",
                                 clean_name=="pattern regularity"~ "temporal patterning (spectral peak)",
                                 clean_name=="Gini index"~ "response time inequality (Gini Index)",
                                 clean_name=="ks statistic"~ "response distribution deviation (K-S statistic)",
                                 clean_name=="ums"~ "filled pauses",
                                 clean_name=="prompt sim trajectory"~ "proximity to prompt trajectory",
                                 clean_name=="reasoning similarity"~ "proximity to reasoning anchors",
                                 clean_name=="spatial similarity"~ "proximity to spatial anchors",
                                 clean_name=="visual similarity"~ "proximity to visual anchors",
                                 grepl("sim",clean_name)~ sub("sim","proximity",clean_name),
                                 grepl("path euclidean distance",clean_name)~ sub("path euclidean distance","trajectory length",clean_name),
                                 grepl("divergence",clean_name)~ sub("divergence","search optimality",clean_name),
                                 grepl("community returns",clean_name)~ sub("community returns","archetype switches",clean_name),
                                 grepl("community",clean_name)~ sub("community","archetype",clean_name),
                                 grepl("communities",clean_name)~ sub("communities","archetype count",clean_name),
                                 clean_name=="archetypal area"~ "archetypal span",
                                 clean_name=="vocabulary volume"~ "volume",
                                 clean_name=="unique word"~ "unique word count",
                                 grepl("origin avg all",clean_name)~sub("origin avg all","(origin: all avg)",clean_name),
                                 grepl("origin avg set",clean_name)~sub("origin avg set","(origin: set avg)",clean_name),
                                 T~clean_name))


lang.features %>% write_rds("data/derivatives/PSVC-features-categorized.rds",compress="gz")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
