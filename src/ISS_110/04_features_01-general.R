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
################################################################################
################################################################################
## performance stats
iss.summ <- iss.tx %>% group_by(te_id, prompt) %>%
  summarise(count = n())

kableExtra::kable(iss.summ %>% pivot_wider(names_from = prompt, values_from = count, id_cols = te_id), 
                  format="html") %>%
  kableExtra::kable_styling(full_width = T, protect_latex = T)

iss.summ %>% 
  ggplot(aes(count)) + geom_histogram() +
  geom_vline(xintercept = c(5,10), linetype=2,color="red") +
  facet_wrap(~prompt, scales = "free") +
  bw.theme
ggsave2("figs/ISS/distributions/word-count-histogram-per-task.pdf",10,8)
################################################################################
################################################################################
################################################################################
## unique word count
iss.tx %>% distinct(te_id, response) %>%
  group_by(te_id) %>% summarise(count_unique_word_all = n()) %>%
  write_rds("data/derivatives/tmp-use/ISS/unique-word-count-per-id.rds")
iss.tx %>% distinct(te_id, prompt, response) %>%
  group_by(te_id, prompt) %>% summarise(count_unique_word_prompt = n()) %>%
  write_rds("data/derivatives/tmp-use/ISS/unique-word-count-per-prompt.rds")
################################################################################
################################################################################
## word productivity
iss.count <- read_rds("data/derivatives/tmp-use/ISS/unique-word-count-per-prompt.rds")
iss.prod <- iss.count %>% 
  pivot_wider(names_from = prompt, values_from = count_unique_word_prompt, id_cols = te_id, values_fill = 0) %>%
  rowwise() %>%
  mutate(letter_productivity = summary(lm(c_across(tolower(ann$prompt[ann$type=="letter"])) ~ c(1:sum(ann$type=="letter"))))$coef[2,1],
         lexical_productivity = summary(lm(c_across(tolower(ann$prompt[ann$type=="lexical"])) ~ c(1:sum(ann$type=="lexical"))))$coef[2,1],
         morph_productivity = summary(lm(c_across(tolower(ann$prompt[ann$type=="morph"])) ~ c(1:sum(ann$type=="morph"))))$coef[2,1],
         abstract_productivity = summary(lm(c_across(tolower(ann$prompt[ann$type=="abstract"])) ~ c(1:sum(ann$type=="abstract"))))$coef[2,1],
         sound_productivity = summary(lm(c_across(tolower(ann$prompt[ann$type=="sound"])) ~ c(1:sum(ann$type=="sound"))))$coef[2,1]) %>%
  select(te_id, ends_with("productivity"))
iss.prod %>% write_rds("data/derivatives/tmp-use/ISS/word-productivity-per-task.rds")
################################################################################
################################################################################
## visual vs. audio
check.hi <- read_rds("data/derivatives/tmp-use/ISS/checkbox-hi-reaction-time.rds")
check.hi %>% group_by(te_id,task) %>% summarise(fastest=min(reaction_time),latest=max(reaction_time),avg=mean(reaction_time))%>%ungroup()%>%
  pivot_longer(cols = c(fastest,latest,avg)) %>% mutate(name=paste(name,task,sep="_")) %>%
  pivot_wider(names_from = name,values_from = value,id_cols = te_id)%>%ungroup()%>%
  mutate(fastest_vis_aud = fastest_check/fastest_hi,latest_vis_aud = latest_check/latest_hi,avg_vis_aud = avg_check/avg_hi)%>%
  select(-contains(c("hi","check")))%>%ungroup()%>%
  write_rds("data/derivatives/tmp-use/ISS/visual-vs-audio-reaction-time-ratio.rds")
################################################################################
################################################################################
################################################################################
## count correct bigrams in SRT

library(tidytext)
l1 <- "didnt the girl paint the picture"
l2 <- "the woman who helps serve the hot lunches at school is the new nurse"
l3 <- "the cafeteria doors around which the seventh graders sometimes stand until the lunch bell rings are always closed but never locked"
prompt.bigrams <- data.frame(p = c(gsub("[^[:alnum:] ]", "", l1),gsub("[^[:alnum:] ]", "", l2),gsub("[^[:alnum:] ]", "", l3)),
                             pred_task = paste0("[RESPONSE", 1:3, "]")) %>%
  unnest_tokens(word, p) %>% group_by(pred_task) %>%mutate(bigram = paste0(word, " ", lead(word))) %>% 
  filter(!is.na(lead(word))) %>%ungroup() %>%mutate(pr = T) %>% group_by(pred_task) %>%mutate(count = n()) %>% ungroup()

# participants
srt.tx.110 <- readxl::read_xlsx("data/RPOE_meta.xlsx",sheet = "ISS_transcription_110B")%>%
  filter(grepl("response",pred_task,ignore.case = T),
         is.na(word_revised)|word_revised !="F")%>%
  mutate(word=ifelse(is.na(word_revised),word,word_revised)) %>%
  select(te_id,word,pred_task) %>% 
  group_by(te_id, pred_task) %>%
  mutate(bigram = paste(word, lead(word), sep = " ")) %>%
  filter(!is.na(lead(word))) %>% ungroup()

## old version bigrams
srt.tx.04 <- readxl::read_xlsx("data/RPOE_meta.xlsx",sheet = "ISS_data_evaluation")[-1,] %>%
  select(te_id,response,pred_task=task) %>% mutate(pred_task=case_when(pred_task=="L1"~"[RESPONSE1]",
                                                                       pred_task=="L2"~"[RESPONSE2]",
                                                                       pred_task=="L3"~"[RESPONSE3]"))%>%
  unnest_tokens(word,response)%>%
  group_by(te_id, pred_task) %>%
  mutate(bigram = paste(word, lead(word), sep = " ")) %>%
  filter(!is.na(lead(word))) %>% ungroup() %>% filter(!te_id %in% srt.tx.110$te_id)

# for each id, get number of bigrams available in prompt bigrams
bigrams <- rbind(srt.tx.110,srt.tx.04) %>% 
  select(te_id, pred_task, bigram) %>% 
  inner_join(prompt.bigrams) %>% 
  group_by(te_id, pred_task) %>%
  dplyr::summarise(accurate_bigram_score = sum(pr,na.rm=T),
                   accurate_bigram_percentage_score = sum(pr,na.rm=T)/count) %>% 
  ungroup() %>% distinct()


# participant 2E_026 didn't responsd to prompt #3
bigrams %>% 
  rbind(data.frame(te_id = c("2E_026"), pred_task = "[RESPONSE3]",accurate_bigram_score = c(0), accurate_bigram_percentage_score = c(0))) %>%
  group_by(te_id) %>% summarise(accurate_bigram_percentage_score=mean(accurate_bigram_percentage_score)) %>%
  mutate(pred_task="all") %>%
  full_join(bigrams)%>%write_rds("data/derivatives/tmp-use/ISS/bigrams.rds",compress = "gz")

################################################################################
################################################################################
################################################################################
