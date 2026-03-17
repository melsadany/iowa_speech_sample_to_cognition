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
psvc.tx <- read_rds("data/derivatives/PSVC-clean-transcription.rds")
################################################################################
################################################################################
################################################################################
## identify new words for embeddings extraction
prev.pwe <- read_rds("data/derivatives/PWE/ALL-embeddings.rds")

new.words <- unique(c(psvc.tx$prompt, psvc.tx$response))
new.words <- new.words[!new.words %in% prev.pwe$text]
write_csv(data.frame(word=new.words),"data/derivatives/PWE/words.csv")
################################################################################
################################################################################
########################## now go run the PWE script ###########################
################################################################################
################################################################################
## load embeddings
emb.all <- read_csv("data/derivatives/PWE/raw-embeddings.csv") %>% as.data.frame() %>%
  rename_at(.vars = vars(-c(word)), .funs = function(x) paste0("Dim",parse_number(x)+1)) %>%
  relocate(word) %>% rename(text=word) %>%
  full_join(prev.pwe)
emb.all %>% write_rds("data/derivatives/PWE/ALL-embeddings.rds", compress="gz")
emb.all <- read_rds("data/derivatives/PWE/ALL-embeddings.rds")
################################################################################
################################################################################
################################################################################
## calculate cosine similarity
##  similarity between pairs: all and consec.
##  similarity to prompt

## extract all possible pairs first
# pairs to prompt
pairs.to.prompt <- psvc.tx %>% distinct(te_id, prompt, response) %>%
  rename(w1=2,w2=3)
# consec. pairs within participant per prompt
consec.pairs <- psvc.tx %>% group_by(te_id, prompt) %>%
  mutate(w1 = lag(response), w2 = response) %>% ungroup() %>%
  select(te_id, prompt, w1,w2) %>% drop_na()
# all possible pairs within participant per prompt
registerDoMC(4)
all.possible.pairs <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- psvc.tx %>% filter(te_id == id.n, prompt == p.n)
    if (nrow(df)>1) {
      do.call(rbind,combn(unique(df$response),m = 2,simplify = F)) %>%
        as.data.frame() %>% rename(w1=1, w2=2) %>%
        mutate(te_id = id.n, prompt = p.n)
    }else {
      return(NULL)
    }
  }
}
# combine all possibilities to run
pairs.to.run.n <- rbind(pairs.to.prompt %>% select(w1,w2),
                        consec.pairs %>% select(w1,w2),
                        all.possible.pairs %>% select(w1,w2)) %>%
  distinct()

# keep new pairs
prev.sim <- read_rds("data/derivatives/tmp-use/PWE_pairs-similarity-all.rds")
pairs.to.run <- left_join(pairs.to.run.n, prev.sim) %>% filter(is.na(cosine_similarity))

# run
if (nrow(pairs.to.run)>0) {
  pairs.sim.n <- foreach(pi=1:nrow(pairs.to.run), .combine = rbind) %dopar% {
    w1 <- pairs.to.run$w1[pi]
    w2 <- pairs.to.run$w2[pi]
    val <- cosine_similarity(emb.all %>% filter(text == w1) %>% select(starts_with("Dim")) %>% as.numeric(),
                             emb.all %>% filter(text == w2) %>% select(starts_with("Dim")) %>% as.numeric())
    data.frame(w1 = w1, w2 = w2, cosine_similarity = val)
  }
  pairs.sim <- rbind(prev.sim, pairs.sim.n)
  write_rds(pairs.sim, "data/derivatives/tmp-use/PWE_pairs-similarity-all.rds", compress = "gz")
  rm(pairs.sim.n)
}
rm(pairs.to.run);rm(pairs.to.run.n);rm(prev.sim)
pairs.sim <- read_rds("data/derivatives/tmp-use/PWE_pairs-similarity-all.rds")

## categorize them into basic categories again
pairs.to.prompt.sim <- left_join(pairs.to.prompt, pairs.sim) %>% rename(prompt=w1) %>%
  group_by(te_id, prompt) %>% summarise(sim_to_prompt = mean(cosine_similarity))
consec.pairs.sim <- left_join(consec.pairs, pairs.sim) %>%
  group_by(te_id, prompt) %>% summarise(consec_pairs_sim = mean(cosine_similarity))
all.possible.pairs.sim <- left_join(all.possible.pairs, pairs.sim) %>%
  group_by(te_id, prompt) %>% summarise(all_pairs_sim = mean(cosine_similarity))

all.pairs.sim.per.prompt <- full_join(pairs.to.prompt.sim, consec.pairs.sim) %>%
  full_join(all.possible.pairs.sim)
write_rds(all.pairs.sim.per.prompt, "data/derivatives/tmp-use/PSVC-PWE-pairs-similarity-per-prompt.rds", compress = "gz")
rm(pairs.to.prompt);rm(consec.pairs);rm(all.possible.pairs);rm(pairs.sim)
rm(pairs.to.prompt.sim);rm(consec.pairs.sim);rm(all.possible.pairs.sim)

# histogram for distribution
all.pairs.sim.per.prompt %>% mutate(task = ifelse(nchar(prompt)==1, "COWAT", "word association")) %>% 
  pivot_longer(cols = -c(task, te_id, prompt)) %>% mutate(name = str_replace_all(name,"_"," ")) %>%
  ggplot(aes(value, fill = name)) +
  geom_histogram(bins = 100) +
  facet_wrap(~task) +
  scale_fill_manual(values = palette.1, name = "") +
  bw.theme +
  labs(x = "cosine similarity", y = "pairs count")
ggsave2("figs/PSVC/distributions/PWE-cos-similarity-histogram.png",width = 6, height = 4)
################################################################################
################################################################################
################################################################################
## calculate euclidean distance per prompt
source("src/utils/calc_euclidean_path.R")
registerDoMC(6)
euc <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  p.res <- foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- psvc.tx %>% filter(te_id == id.n, prompt == p.n)
    if (nrow(df)>1) {
      emb.tmp.0 <- emb.all[which(emb.all$text %in%df$response),]
      rownames(emb.tmp.0) <- emb.tmp.0$text
      data.frame(te_id = id.n, prompt = p.n, count = nrow(df),
                 path_euclidean_distance = calc_euclidean_path(words = df$response, word_embeddings = emb.tmp.0[,-1]))
    } else{
      return(NULL)
    }
  }
}
euc %>% mutate(path_euclidean_distance_normalized = path_euclidean_distance/count) %>%
  select(-count) %>%
  write_rds("data/derivatives/tmp-use/PSVC-PWE-path-euclidean-distance-per-prompt.rds")
################################################################################
################################################################################
################################################################################
## calculate divergence per prompt
source("src/utils/calc_divergence.R")
registerDoMC(6)
div <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  p.res <- foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- psvc.tx %>% filter(te_id == id.n, prompt == p.n)
    if (nrow(df)>2) {
      emb.tmp.0 <- emb.all[which(emb.all$text %in% c(df$response, p.n)),]
      rownames(emb.tmp.0) <- emb.tmp.0$text
      cbind(te_id = id.n, prompt = p.n,
            calc_divergence(words = df$response, word_embeddings = emb.tmp.0[,-1],
                            start_word = p.n)) %>%
        mutate(divergence = actual_distance - optimal_distance,
               divergence_normalized = divergence/nrow(df))
    } else{
      return(NULL)
    }
  }
}
div %>%
  write_rds("data/derivatives/tmp-use/PSVC-PWE-divergence-prompt-starting-per-prompt.rds")
################################################################################
################################################################################
################################################################################
## reduce embeddings to 3D using UMAP
library(umap)
umap.0 <- umap(emb.all %>% select(-text) %>% as.matrix(), 
               n_components = 3, preserve.seed = T)
umap.0 %>% write_rds("data/derivatives/tmp-use/UMAP-on-PWE-word-embedding.rds", compress = "gz")
################################################################################
################################################################################
################################################################################
## calculate vocabulary volume
registerDoMC(6)
umap.0 <- read_rds("data/derivatives/tmp-use/UMAP-on-PWE-word-embedding.rds")
voc.vol <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  df.0 <- psvc.tx %>% filter(te_id == id.n) %>% distinct(response)
  if (nrow(df.0)>3) {
    umap.tmp.00 <- umap.0$layout[which(emb.all$text %in% c(df.0$response)),]
    id.res <- data.frame(te_id = id.n, prompt = "all",
                         vocabulary_volume = cxhull::cxhull(umap.tmp.00)$volume %>% as.numeric())
  }
  
  p.res <- foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- psvc.tx %>% filter(te_id == id.n, prompt == p.n)
    if (nrow(df)>3) {
      umap.tmp.0 <- umap.0$layout[which(emb.all$text %in% df$response),]
      data.frame(te_id = id.n, prompt = p.n,
                 vocabulary_volume = cxhull::cxhull(umap.tmp.0)$volume %>% as.numeric())
    } else{
      return(NULL)
    }
  }
  rbind(id.res,p.res)
}
voc.vol %>%
  write_rds("data/derivatives/tmp-use/PSVC-PWE-vocabulary-volume-per-prompt.rds")
################################################################################
################################################################################
################################################################################
## reduce embeddings to archetypes
library(archetypes)
# archetypes won't word if you have embeddings with 0 sd
arch.n <- 7
archetypes(emb.all[,-c(1,which(apply(emb.all,2,sd)%>%as.numeric()<0.20))] %>% as.matrix(), k = arch.n) %>%
  write_rds("data/derivatives/tmp-use/archetypes-on-PWE-word-embedding.rds")
arch <- (read_rds("data/derivatives/tmp-use/archetypes-on-PWE-word-embedding.rds"))
arch.summ <- archetypes_summ(obj = arch, k = arch.n, points_labels = emb.all$text)

# label text with the highest archetype
text.arch.categorized <- cbind(text = emb.all$text, arch.summ$arch_coef %>% as.data.frame()) %>%
  rename_at(.vars = vars(starts_with("V")), function(x) sub("V","A",x)) %>%
  pivot_longer(cols = -text) %>%
  group_by(text) %>% slice_max(order_by = value)
top.arch.words <- text.arch.categorized %>% group_by(name) %>% slice_max(order_by = value, n = 10)

# simplex
arch.simplex <- arch.summ$simplex_plot %>%
  mutate(lab = ifelse(text %in% top.arch.words$text, T, lab))
arch.simplex %>%
  ggplot(aes(x,y)) +
  geom_segment(aes(x,y,xend = xend, yend = yend), color = "#eef0f2") +
  geom_point(alpha=0.3, shape =1) +
  geom_text(aes(label = ifelse(grepl("A", text), text, ""), size = 0.7), show.legend = F) +
  ggrepel::geom_text_repel(aes(label = ifelse(lab==F|grepl(" ", text), "", text), 
                               size = ifelse(grepl("A", text), 1.5, 0.8)),min.segment.length = 0,
                           max.overlaps = 200, color = "black", force = 0.3, show.legend = F)+
  scale_color_gradient2(high = redblack.col[1], low = "grey")+
  my.guides +
  theme_void()
ggsave2("figs/PSVC/archetypes-map.png", width = 10, height = 10)
write_rds(arch.simplex, "figs/PSVC/obj/PWE-archetypes.simplex.rds", compress = "gz")
################################################################################
################################################################################
################################################################################
## extract archetype count, switches, and lifetime per prompt per participant
source("src/utils/calc_archetypes_visits.R")
registerDoMC(6)
arch.metrics <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  df.0 <- psvc.tx %>% filter(te_id == id.n) %>% distinct(response, .keep_all = T) %>% 
    left_join(text.arch.categorized %>% rename(response=text)) %>% ungroup()
  count.all <- length(unique(df.0$name))
  avg.lifetime.all <- mean((df.0 %>% group_by(name) %>% summarise(lifetime = sum(end-start,na.rm=T)))$lifetime, na.rm=T)
  
  p.res <- foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- df.0 %>% filter(prompt == p.n)
    if (nrow(df)>0) {
      arch.count <- length(unique(df$name))
      avg.lifetime <- mean((df %>% group_by(name) %>% summarise(lifetime = sum(end-start,na.rm=T)))$lifetime)
      if (nrow(df)>1) {
        arch.returns <- calc_archetypes_vistis(words = df$response, archetypes = df$name)
        arch.returns.avg <- mean(arch.returns$returns,na.rm=T)
        arch.returns.max <- max(arch.returns$returns,na.rm=T)
        arch.returns.sum <- sum(arch.returns$returns,na.rm=T)
      } else{
        arch.returns.avg <- NA;arch.returns.max <- NA;arch.returns.sum <- NA
      }
      data.frame(te_id = id.n, prompt = p.n, communities_count = arch.count,
                 community_lifetime = avg.lifetime, 
                 community_returns_avg = arch.returns.avg, 
                 community_returns_max = arch.returns.max,
                 community_returns_sum = arch.returns.sum)
    } else{
      return(NULL)
    }
  }
  full_join(p.res,
            data.frame(te_id = id.n, prompt = "all", communities_count=count.all, community_lifetime = avg.lifetime.all))
}
arch.metrics %>% write_rds("data/derivatives/tmp-use/PSVC-PWE-communities-meta-per-prompt.rds")
################################################################################
################################################################################
################################################################################
## calculate archetypal area
library(geometry)
registerDoMC(6)
arch.area <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  df.0 <- psvc.tx %>% filter(te_id == id.n) %>% distinct(response, .keep_all = T)
  area.all <- convhulln((arch.simplex %>% filter(text %in% df.0$response))[,c("x", "y")], 
                        output.options = T)$area
  
  p.res <- foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- df.0 %>% filter(prompt == p.n)
    if (nrow(df)>2) {
      data.frame(te_id = id.n, prompt = p.n, 
                 archetypal_area = convhulln((arch.simplex %>% filter(text %in% df$response))[,c("x", "y")], 
                                             output.options = T)$area)
    } else{
      return(NULL)
    }
  }
  full_join(p.res,data.frame(te_id = id.n, prompt = "all", archetypal_area=area.all))
}
arch.area %>% write_rds("data/derivatives/tmp-use/PSVC-PWE-archetypal-area-per-prompt.rds")
################################################################################
################################################################################
################################################################################
## calculate archetypal richness
arch.weights <- cbind(text = emb.all$text, arch.summ$arch_coef %>% as.data.frame()) %>%
  rename_at(.vars = vars(starts_with("V")), function(x) sub("V","A",x))
registerDoMC(6)
arch.richness <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  df.0 <- psvc.tx %>% filter(te_id == id.n) %>% distinct(response, .keep_all = T)
  richness.all <- sum(left_join(df.0, arch.weights%>%rename(response=text)) %>%
                        summarise_at(.vars = vars(starts_with("A")), .funs = function(x) max(x)) %>% 
                        as.numeric())
  
  p.res <- foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- df.0 %>% filter(prompt == p.n)
    if (nrow(df)>0) {
      data.frame(te_id = id.n, prompt = p.n, 
                 archetypal_richness = sum(left_join(df, arch.weights%>%rename(response=text)) %>%
                                             summarise_at(.vars = vars(starts_with("A")), .funs = function(x) max(x)) %>% 
                                             as.numeric()))
    }else{
      return(NULL)
    }
  }
  full_join(p.res,data.frame(te_id = id.n, prompt = "all", archetypal_richness=richness.all))
}
arch.richness %>% write_rds("data/derivatives/tmp-use/PSVC-PWE-archetypal-richness-per-prompt.rds")
################################################################################
################################################################################
################################################################################
## calculate l1 and l2 norms per word
# raw
norms.raw <- data.frame(text=emb.all$text,
                        l1_norm=rowSums(emb.all[,-1]),
                        l2_norm = sqrt(rowSums(emb.all[,-1]^2)))
## normalized to an "origin"
# 1. origin=average of all
norms.origin.av.all <- data.frame(text=emb.all$text,
                                  l1_norm_origin_avg_all=rowSums(emb.all[,-1]-colMeans(emb.all[,-1])),
                                  l2_norm_origin_avg_all = sqrt(rowSums((emb.all[,-1]-colMeans(emb.all[,-1]))^2)))
# 2. origin=average of words (a, the)
norms.origin.av.set <- data.frame(text=emb.all$text,
                                  l1_norm_origin_avg_set=rowSums(emb.all[,-1]-colMeans((emb.all%>%filter(text%in%c("a","the")))[,-1])),
                                  l2_norm_origin_avg_set = sqrt(rowSums((emb.all[,-1]-colMeans((emb.all%>%filter(text%in%c("a","the")))[,-1]))^2)))
norms <- inner_join(norms.raw,norms.origin.av.all)%>%inner_join(norms.origin.av.set)

# combine
participant.norms <- psvc.tx %>% distinct(te_id, prompt, response) %>%
  left_join(norms %>% rename(response=text)) %>%
  group_by(te_id, prompt) %>%
  summarise(l1_norm = mean(l1_norm),l2_norm = mean(l2_norm),
            l1_norm_origin_avg_all = mean(l1_norm_origin_avg_all),l2_norm_origin_avg_all = mean(l2_norm_origin_avg_all),
            l1_norm_origin_avg_set = mean(l1_norm_origin_avg_set),l2_norm_origin_avg_set = mean(l2_norm_origin_avg_set)) %>%
  rbind(psvc.tx %>% distinct(te_id, response) %>%
          left_join(norms %>% rename(response=text)) %>%
          group_by(te_id) %>%
          summarise(l1_norm = mean(l1_norm),l2_norm = mean(l2_norm),
                    l1_norm_origin_avg_all = mean(l1_norm_origin_avg_all),l2_norm_origin_avg_all = mean(l2_norm_origin_avg_all),
                    l1_norm_origin_avg_set = mean(l1_norm_origin_avg_set),l2_norm_origin_avg_set = mean(l2_norm_origin_avg_set)) %>%
          mutate(prompt="all"))
participant.norms %>% write_rds("data/derivatives/tmp-use/PSVC-PWE-norms-per-prompt.rds")
################################################################################
################################################################################
################################################################################
## prompt adherance changes
# calculate how similarity to prompt changes over time
pairs.sim <- read_rds("data/derivatives/tmp-use/PWE_pairs-similarity-all.rds")
prompt.sim.traj <- pairs.to.prompt %>% 
  left_join(psvc.tx %>% distinct(te_id, prompt, response, .keep_all = TRUE) %>%
              select(te_id, w1 = prompt, w2 = response, start)) %>%
  left_join(pairs.sim) %>% group_by(te_id, w1) %>%
  summarise(prompt_sim_trajectory = tryCatch({coef(lm(cosine_similarity ~ start))[2]}, 
                                             error = function(e) NA_real_),
            n_obs = n(),.groups = 'drop')
prompt.sim.traj %>% write_rds("data/derivatives/tmp-use/PSVC-PWE-prompt-similarity-trajectory-per-prompt.rds",
                              compress = "gz")
################################################################################
################################################################################
################################################################################
################################################################################
