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
## read tx
iss.tx <- read_rds("data/derivatives/ISS-clean-transcription.rds")
pload("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")
ann2 <- ann%>%filter(task=="WORD_ASSOC")%>%select(prompt,type)%>%mutate_all(.funs = function(x)tolower(x))
## read general features
check.hi <- read_rds("data/derivatives/tmp-use/ISS/checkbox-hi-reaction-time.rds")
vis.aud <- read_rds("data/derivatives/tmp-use/ISS/visual-vs-audio-reaction-time-ratio.rds")
comments <- read_rds("data/derivatives/tmp-use/ISS/comments-count.rds")
rep.prompt <- read_rds("data/derivatives/tmp-use/ISS/repeated-prompt-count.rds")
rep.word.prompt <- read_rds("data/derivatives/tmp-use/ISS/repeated-words-per-prompt-count.rds")
word.count <- rbind(read_rds("data/derivatives/tmp-use/ISS/unique-word-count-per-id.rds")%>%rename(unique_word_count=2)%>%mutate(prompt="all"),
                    read_rds("data/derivatives/tmp-use/ISS/unique-word-count-per-prompt.rds")%>%rename(unique_word_count=3))
wmem.scores <- read_rds("data/derivatives/tmp-use/ISS/wmem-scores.rds")
product <- read_rds("data/derivatives/tmp-use/ISS/word-productivity-per-task.rds")
bigrams <- read_rds("data/derivatives/tmp-use/ISS/bigrams.rds")%>%rename(prompt=3)
word.reading <- read_rds("data/derivatives/tmp-use/ISS/word-reading-response-times.rds")

app.feat <- read_rds("data/derivatives/ISS_app--based-features.rds")
################################################################################
################################################################################
################################################################################
per.type <- full_join(check.hi %>% group_by(te_id,task) %>% summarise(fastest=min(reaction_time),latest=max(reaction_time),avg=mean(reaction_time))%>%ungroup()%>%
                        pivot_longer(cols = c(fastest,latest,avg)) %>% mutate(name=paste(name,task,sep="_")) %>%
                        pivot_wider(names_from = name,values_from = value,id_cols = te_id)%>%ungroup(),
                      vis.aud) %>%
  full_join(comments%>%inner_join(ann2) %>% group_by(type,te_id) %>% summarise(count=sum(comment_count)) %>%ungroup() %>%
              pivot_wider(names_from = type,values_from = count,id_cols = te_id) %>% rename_at(.vars = vars(-c(te_id)),.funs = function(x)paste0(x,"__comment_count"))%>%ungroup()) %>%
  full_join(rep.prompt%>%inner_join(ann2) %>% group_by(type,te_id) %>% summarise(val=sum(rep_prompt)) %>%ungroup() %>%
              pivot_wider(names_from = type,values_from = val,id_cols = te_id) %>% rename_at(.vars = vars(-c(te_id)),.funs = function(x)paste0(x,"__rep_prompt"))) %>%
  full_join(rep.word.prompt%>%inner_join(ann2) %>% group_by(type,te_id) %>% summarise(val=sum(rep_words_per_prompt)) %>%ungroup() %>%
              pivot_wider(names_from = type,values_from = val,id_cols = te_id) %>% rename_at(.vars = vars(-c(te_id)),.funs = function(x)paste0(x,"__rep_words_per_prompt"))) %>%
  full_join(word.count%>%left_join(ann2)%>% group_by(type,te_id) %>% summarise(val=sum(unique_word_count)) %>%ungroup() %>%mutate(type=ifelse(is.na(type),"ALL",type))%>%
              pivot_wider(names_from = type,values_from = val,id_cols = te_id) %>% rename_at(.vars = vars(-c(te_id)),.funs = function(x)paste0(x,"__unique_word_count"))) %>%
  full_join(wmem.scores%>%rename_at(.vars = vars(-1),.funs = function(x)paste0(str_replace_all(x," ","_"),"__wmem_score")))%>%
  full_join(product %>% rename_at(.vars=vars(-te_id),.funs = function(x) str_replace_all(x,"_","__")))%>%
  full_join(word.reading%>%group_by(te_id,type)%>%summarise(avg=mean(reaction_time))%>%ungroup()%>%
              pivot_longer(cols = c(avg)) %>% mutate(name=paste(name,type,sep="_")) %>%
              pivot_wider(names_from = name,values_from = value,id_cols = te_id)%>%ungroup()) %>%
  full_join(reduce(app.feat$per_type, full_join) %>% select(-c(unique_word_count)) %>% rename(te_id=participant_id) %>%
              pivot_longer(cols=-c(te_id,type)) %>% mutate(name = paste0(type,"__",name)) %>%
              pivot_wider(names_from=name,values_from=value, id_cols=te_id))


iss.all <- list("WAT_tx"=iss.tx,
                "raw_features" = list("check_hi"=check.hi,"vis_aud_ratio"=vis.aud,"comments"=comments,
                                      "rep_prompt"=rep.prompt,"rep.word.prompt"=rep.word.prompt,
                                      "word_count"=word.count,"wmem_scores"=wmem.scores,
                                      "productivity"=product,"bigram_accuracy"=bigrams,
                                      "word_reading_time"=word.reading,
                                      word_assoc_features = reduce(app.feat$per_type, full_join) %>% select(-c(unique_word_count)) %>% rename(te_id=participant_id) %>%
                                        pivot_longer(cols=-c(te_id,type)) %>% mutate(name = paste0(type,"__",name)) %>%
                                        pivot_wider(names_from=name,values_from=value, id_cols=te_id)),
                "per_type"=per.type)
iss.all%>%write_rds("data/derivatives/ISS_USE-THIS.rds")
iss.all <- read_rds("data/derivatives/ISS_USE-THIS.rds")
################################################################################
################################################################################
data.frame(feature=c(colnames(iss.all$per_type)[-1],colnames(bigrams)[2]))%>%
  mutate(cat = case_when(grepl("check",feature)~"visual",
                         grepl("_hi",feature)~"audio",
                         grepl("rep|comment",feature)~"adherence",
                         grepl("unique_word",feature)~"general",
                         grepl("wmem|bigram",feature)~"memory",
                         grepl("productivity",feature)~"trajectory",
                         grepl("negative|positive|neutral",feature)~"word reading latency"),
         feature_clean = str_replace_all(feature,"-|_"," "),
         source="ISS 1.10B") %>%
  write_rds("data/derivatives/ISS-features-categorized.rds")

################################################################################
################################################################################
