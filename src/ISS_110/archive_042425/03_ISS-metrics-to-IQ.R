################################################################################
#            analyzing ISS features to predict IQ and NIH-TB scores            #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files here
m1.m2 <- read_rds("data/derivatives/m1m2-age-sex-corrected.rds") 
m1.m2.all <- read_rds("data/derivatives/m1m2-all-all.rds") 
demo <- read_rds("data/raw/demo.rds")
ps.vc <- read_rds("data/derivatives/ps-vc-text-clean.rds")
#####
iss.feat <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 14) %>%
  drop_na(te_id) %>%
  mutate_at(.vars = c(4:10,12), .funs = function(x) as.numeric(x)) %>%
  rename(prompt3_section_3_score = prompt3_section_3) %>%
  left_join(demo)
################################################################################
################################################################################
################################################################################
# extract the bigrams and calculate their accuracy
l1 <- "didn't the girl paint the picture"
l2 <- "the woman who helps serve the hot lunches at school is the new nurse"
l3 <- "the cafeteria doors, around which the seventh graders sometimes stand until the lunch bell rings, are always closed but never locked"
library(tidytext)
dd <- iss.feat %>%
  # filter(task=="L3") %>%
  select(te_id, task, response) %>%
  mutate(response = tolower(response),
         response = str_remove_all(pattern = "\\\n", response),
         response = str_remove_all(pattern = "\\\r", response),
         response = gsub("[^[:alnum:] ]", "", response)) %>%
  unnest_tokens(word, response)
# Generate bigrams
dd.bigrams <- dd %>%
  group_by(te_id) %>%
  mutate(bigram = paste0(word, " ", lead(word))) %>%
  filter(!is.na(lead(word))) %>%
  ungroup()
prompt.bigrams <- data.frame(p = c(gsub("[^[:alnum:] ]", "", l1),
                                   gsub("[^[:alnum:] ]", "", l2),
                                   gsub("[^[:alnum:] ]", "", l3)),
                             task = paste0("L", 1:3)) %>%
  unnest_tokens(word, p) %>%
  mutate(bigram = paste0(word, " ", lead(word))) %>%
  filter(!is.na(lead(word))) %>%
  mutate(pr = T) %>%
  group_by(task) %>%
  mutate(count = n()) %>% ungroup()
# for each id, get number of bigrams available in prompt bigrams
bigrams.2 <- dd.bigrams %>% 
  select(-word) %>% 
  left_join(prompt.bigrams) %>% filter(pr == T) %>%
  group_by(te_id, task) %>%
  drop_na(bigram) %>%
  dplyr::summarise(accurate_bigram_score = n(),
                   accurate_bigram_percentage_score = n()/count) %>% 
  ungroup() %>% distinct()
####################
# # get cosine similarity between prompt and response for task L3
# library(text)
# res <- iss.feat %>% 
#   filter(task == "L3") %>%
#   select(te_id, response)
# e <- textEmbed(c(res$response, l3))
# emb <- cbind(text = c(res$response, l3),
#              e$texts$texts)
# registerDoMC(cores = 4)
# iss.cos <- foreach(i = 1:nrow(res), .combine = rbind) %dopar% {
#   p <- res$te_id[i]
#   pt <- res$response[i]
#   pr <- emb %>% filter(text == l3) %>% select(-text)
#   re <- emb %>% filter(text == pt) %>% select(-text)
#   cs <- text::textSimilarity(pr,re, method = "cosine")
#   df <- data.frame(te_id = p,
#                    SRT_cosine_similarity_score = cs)
#   return(df)
# }
####################
####################
# combine the bigram accurace features with the rest
iss.feat <- left_join(iss.feat, bigrams.2)
# iss.feat <- left_join(iss.feat, bigrams.2) %>%
#   left_join(iss.cos) %>%
#   mutate(SRT_cosine_similarity_score = as.numeric(SRT_cosine_similarity_score)) %>%
#   select(-SRT_cosine_similarity_score)
write_rds(iss.feat, "data/derivatives/ISS-features.rds")
################################################################################
################################################################################
################################################################################
# plot distributions, and correlations with demo
iss.feat %>%
  pivot_longer(cols = c(ends_with("score"), starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = as.factor(value)) %>%
  filter(feature != "accurate_bigram_percentage_score") %>%
  ggplot(aes(x = value)) +
  geom_bar() +
  ggh4x::facet_grid2(rows = vars(task), cols = vars(feature), scales = "free",independent = T)+
  labs(x="score")
ggsave("figs/ISS/distribution-of-features.png", bg = "white",
       width = 14, height = 8, units = "in", dpi = 360)
#####
# check corr with demo
iss.feat %>%
  pivot_longer(cols = c(ends_with("score"), starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = factor(value, levels = c(0:20))) %>%
  filter(feature != "accurate_bigram_percentage_score") %>%
  ggplot(aes(x = value, fill = sex)) +
  geom_bar() +
  scale_fill_manual(values = redblu.col) +
  ggh4x::facet_grid2(rows = vars(task), cols = vars(feature), scales = "free",independent = T)+
  labs(x="score")
ggsave("figs/ISS/distribution-of-features-w-sex-filling.png", bg = "white",
       width = 14, height = 8, units = "in", dpi = 360)
#####
# check age correlation?
iss.feat %>%
  pivot_longer(cols = c(ends_with("score"), starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = factor(value, levels = c(0:20))) %>%
  filter(feature != "accurate_bigram_percentage_score") %>%
  ggplot(aes(x = value, y = age)) +
  geom_boxplot() +
  ggpubr::stat_compare_means(color = "red")+
  ggh4x::facet_grid2(rows = vars(task), cols = vars(feature), scales = "free",independent = T)+
  labs(x="score")
ggsave("figs/ISS/corr-of-features-w-age.png", bg = "white",
       width = 16, height = 8, units = "in", dpi = 360)
################################################################################
################################################################################
# correlate these scores with wm
# binary score from L2
p1 <- iss.feat %>%
  filter(task == "L2") %>%
  mutate(binary_score = as.factor(binary_score)) %>%
  left_join(m1.m2 %>% rename(devGenes_id = dev_id)) %>%
  pivot_longer(cols = c(
    VCI_composite_score, PSI_composite_score,
    WM_composite_score, Digit_Span, picture_sequence_memory_test_age_corrected_standard_score),
    names_to = "iq", values_to = "val2") %>%
  filter(iq %in% c("WM_composite_score", "Digit_Span", "VCI_composite_score")) %>%
  group_by(iq) %>% mutate(val2 = scale(val2, scale = T, center = T)[,1]) %>% ungroup() %>%
  ggplot(aes(x = binary_score, y = val2, fill=binary_score)) +
  # see::geom_violindot()+
  geom_violin(show.legend = F) +
  geom_boxplot(width = 0.2, fill="white") + ggpubr::stat_compare_means(color = "red", label.y = 2)+
  scale_fill_manual(values = c("#8b8589", "#36454f")) +
  ggh4x::facet_grid2(cols = vars(iq), scales = "free_x",independent = "x")+
  labs(y="Z-transformed IQ score", x = "binary score of repetition", 
       title = "correlations from SRT L2 data") +
  theme_linedraw() +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
p1
# scatterplot for L3 with errors of substitution and omission & scaled_score
p2 <- iss.feat %>%
  filter(task == "L3") %>%
  mutate(binary_score = ifelse(scaled_score>0, 1, 0)) %>%
  pivot_longer(cols = c(binary_score, scaled_score, starts_with("error"), accurate_bigram_score, prompt3_section_3_score), 
               names_to = "feature") %>%
  # mutate(value = factor(value, levels = c(0:17))) %>%
  mutate(value = as.numeric(value)) %>%
  left_join(m1.m2) %>%
  pivot_longer(cols = c(
    VCI_composite_score, PSI_composite_score,
    WM_composite_score, Digit_Span, picture_sequence_memory_test_age_corrected_standard_score),
    names_to = "iq", values_to = "val2") %>%
  filter(iq %in% c("Digit_Span", "WM_composite_score", "VCI_composite_score"),
         feature %in% c("scaled_score", "error_omission", "error_substitution", "accurate_bigram_score")) %>%
  group_by(iq) %>% mutate(val2 = scale(val2, scale = T, center = T)[,1]) %>% ungroup() %>%
  ggplot(aes(y = value, x = val2)) +
  geom_point(position = "jitter") + geom_smooth(method = "lm", color = six.colors[3]) + ggpubr::stat_cor(color = "red") +
  ggh4x::facet_grid2(cols = vars(iq), rows = vars(feature), scales = "free")+
  geom_rug(alpha = 0.6) +
  theme_linedraw() +
  labs(y="feature score", x = "Z-transformed IQ score",
       title = "correlations from SRT L3 data")
patchwork::wrap_plots(p2,
                      p1,
                      ncol = 1, heights = c(4,1))
ggsave("figs/ISS/corr-of-features-w-IQ.png", bg = "white",
       width = 8, height = 10, units = "in", dpi = 360)
#####
# average by id, and check correlations
iss.feat %>%
  group_by(te_id) %>% dplyr::summarise_at(.vars = c(colnames(iss.feat)[c(4:10, 18)]),
                                          .funs = function(x) mean(x, na.rm = T)) %>%
  pivot_longer(cols = c(binary_score, scaled_score, accurate_bigram_percentage_score,
                        error_omission, error_substitution), 
                        # starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = as.numeric(value)) %>%
  left_join(m1.m2) %>% drop_na(dev_id) %>%
  pivot_longer(cols = c(
    VCI_composite_score, PSI_composite_score,
    WM_composite_score, Digit_Span, list_sorting_wm_age_corrected_standard_score),
    names_to = "iq", values_to = "val2") %>%
  group_by(iq) %>% mutate(val2 = scale(val2, scale = T, center = T)[,1]) %>% ungroup() %>%
  group_by(feature) %>% mutate(value = scale(value, scale = T, center = T)[,1]) %>% ungroup() %>%
  ggplot(aes(y = value, x = val2)) +
  geom_point(position = "jitter") + geom_smooth(method = "lm", color = six.colors[3]) + ggpubr::stat_cor(color = "red") +
  ggh4x::facet_grid2(cols = vars(iq), rows = vars(feature), scales = "free")+
  geom_rug(alpha = 0.6) +
  theme_linedraw() +
  labs(y="Z-transformed feature score", x = "Z-transformed IQ score",
       title = "correlations from averaged SRT data")
ggsave("figs/ISS/corr-of-features-w-IQ-extras-averaged-by-id.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################