################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/workbench/customized-functions/correct_path.R"))
source(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis")
setwd(project.dir)
################################################################################
################################################################################
pload("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")
iss.meta.me <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "ISS-metadata") %>%
  filter(task_v=="1.10B")
m1m2 <- read_rds("data/m1m2.rds")
m1m2.s <- read_rds("data/m1m2-sex-corrected.rds")
################################################################################
################################################################################
# read the cleaned tx
clean.tx <- read_csv("data/derivatives/ISS_transcription/110B-tx-clean.csv")
################################################################################
################################################################################
## extract responses by task and drop prompts said out loud
# word association
wa.tx <- clean.tx %>% 
  filter(pred_task %in% ann$prompt[ann$task=="WORD_ASSOC"])
# SRT
#   second batch
srt.tx <- clean.tx %>% 
  filter(grepl("RESPONSE[1-3]", pred_task))
#   1st batch
srt.01 <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "ISS_data_evaluation",
                            range = "A3:C143",col_names = c("te_id", "task", "response")) %>%
  mutate(pred_task = paste0(sub("L", "[RESPONSE",task),"]"),
         response = tolower(gsub("[^[:alnum:] ]", "", response))) %>% select(-task)
################################################################################
################################################################################
################################################################################
################################################################################
## count how many correct answers in word memory
mem.count <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "ISS_transcription_110B") %>%
  filter(wmem_correct=="T") %>%
  group_by(te_id) %>%
  dplyr::summarise(correct_mem_count = n())

mem.count %>% mutate(correct_mem_ratio = (correct_mem_count/4)) %>% 
  inner_join(m1m2) %>%
  pivot_longer(col = colnames(m1m2.s)[-c(1:2)]) %>%
  ggplot(aes(x = value, y = correct_mem_count)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = 2)+
  ggpubr::stat_cor() +
  facet_wrap(~name, scales= "free") +
  bw.theme
mem.count %>% mutate(correct_mem_ratio = (correct_mem_count/4)) %>% 
  mutate(correct_mem_count = as.factor(correct_mem_count)) %>%
  inner_join(m1m2.s) %>%
  pivot_longer(col = colnames(m1m2.s)[-c(1:2)]) %>%
  ggplot(aes(x = correct_mem_count, y = value)) +
  geom_boxplot() +
  ggpubr::stat_compare_means(label.y.npc = 0.95) +
  facet_wrap(~name, scales= "free") +
  bw.theme
ggsave2("figs/ISS/corr-of-prompt-mem-w-IQ.png", width = 14,height = 12)
################################################################################
################################################################################
## count words in word association task
wa.count <- wa.tx %>%
  group_by(te_id, pred_task) %>%
  summarise(count = n())

## get the onset for word association
iss.start <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "ISS_det") %>%
  filter(task_version=="1.10B") %>%
  mutate(start_in_sec = (as.numeric(start_min)*60) + as.numeric(start_sec)) %>%
  select(te_id, start_in_sec) %>%
  mutate(diff1 = start_in_sec - 34)
ann %>% select(prompt, seconds) %>% mutate(seconds = seconds-34)



wa.task.start.time <- clean.tx %>% filter(pred_task=="wmem") %>% 
  group_by(te_id) %>% slice_head(n = 1) %>% mutate(diff = 77 - start) %>%
  select(te_id, diff)

wa.onset <- wa.tx %>%
  left_join(iss.start) %>% mutate(start = start-diff1) %>% 
  left_join(ann[20:41,] %>% mutate(ann_task_start = seconds-34)%>% select(pred_task=prompt,ann_task_start)) %>%
  mutate(time_gap_to_task_start = start - ann_task_start) %>% 
  group_by(te_id, pred_task) %>% slice_head(n = 1)%>% View

################################################################################
################################################################################
## count correct bigrams in SRT
library(tidytext)
l1 <- "didnt the girl paint the picture"
l2 <- "the woman who helps serve the hot lunches at school is the new nurse"
l3 <- "the cafeteria doors around which the seventh graders sometimes stand until the lunch bell rings are always closed but never locked"
prompt.bigrams <- data.frame(p = c(gsub("[^[:alnum:] ]", "", l1),
                                   gsub("[^[:alnum:] ]", "", l2),
                                   gsub("[^[:alnum:] ]", "", l3)),
                             pred_task = paste0("[RESPONSE", 1:3, "]")) %>%
  unnest_tokens(word, p) %>% group_by(pred_task) %>%
  mutate(bigram = paste0(word, " ", lead(word))) %>% 
  filter(!is.na(lead(word))) %>%ungroup() %>%
  mutate(pr = T) %>% group_by(pred_task) %>%
  mutate(count = n()) %>% ungroup()

# participants
bigrams.r <- rbind(srt.tx %>% select(-c(start,end)),
                   srt.01 %>% unnest_tokens(word, response)) %>% 
  group_by(te_id, pred_task) %>%
  mutate(bigram = paste(word, lead(word), sep = " ")) %>%
  filter(!is.na(lead(word))) %>% ungroup()

# for each id, get number of bigrams available in prompt bigrams
bigrams <- bigrams.r %>% 
  select(te_id, pred_task, bigram) %>% 
  inner_join(prompt.bigrams) %>% filter(pr == T) %>%
  group_by(te_id, pred_task) %>%
  drop_na(bigram) %>%
  dplyr::summarise(accurate_bigram_score = n(),
                   accurate_bigram_percentage_score = n()/count) %>% 
  ungroup() %>% distinct()

# participant 2E_026 didn't responsd to prompt #3
bigrams <- rbind(bigrams,
                 data.frame(te_id = c("2E_026"), pred_task = "[RESPONSE3]",
                            accurate_bigram_score = c(0), accurate_bigram_percentage_score = c(0)))
write_rds(bigrams, "data/derivatives/ISS-bigrams.rds",compress = "gz")


rpoe.all <- read_rds("data/all-summarized-data.rds")
inner_join(rpoe.all %>% select(te_id, APS),
           bigrams) %>%
  ggplot(aes(x = APS, y = log2(accurate_bigram_percentage_score))) +
  geom_point(shape = 1) + geom_smooth(method = "lm") +
  ggpubr::stat_cor() +
  facet_wrap(~pred_task) +
  bw.theme

################################################################################
################################################################################
## combine all features and save
iss.all <- full_join(wa.count %>% mutate(feature = "word_count") %>% rename(value = count),
                     mem.count %>% mutate(feature = "correct_mem_count") %>% rename(value = correct_mem_count)) %>%
  full_join(wa.onset)
################################################################################
################################################################################
################################################################################
p1 <- wa.count %>%
  inner_join(m1m2 %>% select(te_id, FSIQ)) %>% drop_na() %>%
  ggplot(aes(x = FSIQ, y = count)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", color = six.colors[3]) +
  ggpubr::stat_cor() +
  facet_wrap(~pred_task, scales = "free") +
  bw.theme

p2 <- clean.tx %>%
  filter(pred_task %in% ann$prompt[ann$task=="WORD_ASSOC"]) %>%
  distinct(te_id,word) %>%
  group_by(te_id) %>%
  dplyr::summarise(count = n()) %>%
  inner_join(m1m2 %>% select(te_id, FSIQ)) %>% drop_na() %>%
  ggplot(aes(x = FSIQ, y = count)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm", color = six.colors[3]) +
  ggpubr::stat_cor() +
  bw.theme + labs(title = "total number of unique words in the word association tasks")
patchwork::wrap_plots(p1,patchwork::wrap_plots(p2,patchwork::plot_spacer(), nrow = 1, widths = c(1,4)),
                      ncol = 1,heights = c(4,1))
ggsave2("figs/ISS/corr-of-features-w-IQ_110B.png", width = 12, height = 10)


tt <- wa.count %>%
  left_join(ann %>% select(pred_task = prompt, type)) %>%
  group_by(te_id, type) %>% summarise(count = sum(count)) %>%
  pivot_wider(names_from = type, values_from = count, id_cols = te_id) %>%ungroup() %>%
  inner_join(m1m2.s)
corr.table(tt %>% select(2:6),
           tt %>% select(colnames(m1m2.s)[-c(1:2)])) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(tt)[2:6], V2 %in% colnames(m1m2.s)) %>%
  mutate(cat = ifelse(grepl("age_corrected", V2), "NIH-TB", "IQ"),
         V2 = sub("_age_corrected_standard_score", "", V2),
         V2 = factor(V2, levels = sub("_age_corrected_standard_score", "", colnames(m1m2.s)[-c(1:2)]))) %>%
  ggplot(aes(x = V1, y = V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval < 0.05, "*", "")))) +
  geom_tile() + geom_text() +
  ggh4x::facet_grid2(rows = vars(cat), scales = "free", space = "free") +
  redblack.col.gradient + my.guides +
  null_labs + bw.theme

tt %>%
  pivot_longer(cols = colnames(m1m2.s)[-c(1:2)]) %>%
  ggplot(aes(x = value, y = count)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor() +
  ggh4x::facet_grid2(rows = vars(type), cols = vars(name), 
                     scales = "free", space = "free") +
  bw.theme

################################################################################
################################################################################
write_rds(wa.count, "data/derivatives/ISS_transcription/word-count-wa.rds", compress = "gz")
################################################################################
################################################################################
## extract word sentiment
vc.analyzed <- cbind(vc.transcription,
                     nrc = syuzhet::get_nrc_sentiment(vc.transcription$text),
                     sentimentr::profanity(vc.transcription$text)%>%select(profanity_count),
                     lingmatch = lingmatch::lma_meta(vc.transcription$text),
                     lingmatch = lingmatch::lma_termcat(vc.transcription$text)) %>%
  select(-c(lingmatch.words, lingmatch.unique_words, lingmatch.clauses, lingmatch.sentences, 
            lingmatch.words_per_clause,
            lingmatch.words_per_sentence, lingmatch.characters_per_word, lingmatch.syllables_per_word,
            lingmatch.type_token_ratio))
## count of words per prompt
wa.tx <- clean.tx %>%
  filter(pred_task %in% ann$prompt[ann$task=="WORD_ASSOC"]) %>%
  distinct(te_id,pred_task,word) %>%
  group_by(te_id,pred_task) %>%
  dplyr::summarise(count = n())

################################################################################
################################################################################