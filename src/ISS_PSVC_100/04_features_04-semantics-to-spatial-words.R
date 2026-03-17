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
## read embeddings
load("data/derivatives/tmp-use/word-embedding-from-text-package.rda")
################################################################################
################################################################################
## define words of interest
visual_anchors <- c("rotate", "pattern", "space", "shape", "design", "arrange", "symmetry", "grid", "angle", "dimension")
spatial_anchors <- c("above", "below", "left", "right", "inside", "outside","between", "beside", "corner", "edge")
reasoning_anchors <- c("logic", "rule", "pattern", "sequence", "relation","analogy", "correspond", "transform")
anchors.meta <- tribble(~anchor_category, ~text,
                        "visual", visual_anchors,
                        "spatial", spatial_anchors,
                        "reasoning", reasoning_anchors) %>% unnest(text)
################################################################################
################################################################################
################################################################################
## calculate cosine similarity to reference words
pairs.to.run <- data.frame(w1 = anchors.meta$text, w2 = rep(unique(psvc.tx$response), each = nrow(anchors.meta)))
registerDoMC(8)
pairs.sim <- foreach(pi=1:nrow(pairs.to.run), .combine = rbind) %dopar% {
  w1 <- pairs.to.run$w1[pi]
  w2 <- pairs.to.run$w2[pi]
  val <- cosine_similarity(emb.all %>% filter(text == w1) %>% select(starts_with("Dim")) %>% as.numeric(),
                           emb.all %>% filter(text == w2) %>% select(starts_with("Dim")) %>% as.numeric())
  data.frame(w1 = w1, w2 = w2, cosine_similarity = val)
}
pairs.sim %>% write_rds("data/derivatives/tmp-use/similarity-to-anchor-set.rds", compress = "gz")
pairs.sim <- read_rds("data/derivatives/tmp-use/similarity-to-anchor-set.rds")

# group by prompt and anchor
psvc.anchors.sim <- left_join(psvc.tx %>% select(te_id, prompt, response),
                              left_join(anchors.meta, pairs.sim %>% rename(text=w1, response=w2))) %>%
  group_by(te_id, prompt, anchor_category) %>% summarise(val = max(cosine_similarity, na.rm=T)) %>%
  mutate(anchor_category = paste0(anchor_category, "_similarity")) %>%
  pivot_wider(names_from = anchor_category, values_from = val)
psvc.anchors.sim %>% write_rds("data/derivatives/tmp-use/PSVC-similarity-to-anchor-sets-per-prompt.rds", compress = "gz")
################################################################################
################################################################################