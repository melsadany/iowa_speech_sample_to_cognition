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
################################################################################
################################################################################
################################################################################
## extract response onset
psvc.onset <- psvc.tx %>%
  group_by(te_id, prompt) %>% 
  slice_min(order_by = start, n=1) %>%
  select(te_id, prompt, onset = start)
write_rds(psvc.onset, "data/derivatives/tmp-use/PSVC-onset-per-prompt.rds")

################################################################################
################################################################################
## going over the task time
psvc.last.resp <- psvc.tx %>%
  group_by(te_id, prompt) %>% 
  slice_max(order_by = end,n = 1) %>%
  mutate(task_time = ifelse(nchar(prompt)==1,30,12),
         last_response_gap = end-task_time) %>%
  select(te_id, prompt, last_response_gap)
write_rds(psvc.last.resp, "data/derivatives/tmp-use/PSVC-last-response-gap-per-prompt.rds")

################################################################################
################################################################################
## thinking time between consec. pairs
psvc.thinking <- psvc.tx %>% rownames_to_column("pos") %>% group_by(te_id, prompt) %>%
  filter(start>0) %>%
  arrange(te_id, prompt, start,.by_group = T) %>%
  mutate(count = n(), pos = as.numeric(pos)) %>% filter(count>1) %>%
  mutate(thinking_time_e = start - lag(end),
         thinking_time_s = start - lag(start),
         thinking_time = ifelse(thinking_time_e < 0, thinking_time_s, thinking_time_e)) %>%
  summarise(thinking_time_min = min(thinking_time,na.rm=T),
            thinking_time_mean = mean(thinking_time,na.rm=T),
            thinking_time_max = max(thinking_time,na.rm=T),
            thinking_time_sd = sd(thinking_time,na.rm=T))
write_rds(psvc.thinking, "data/derivatives/tmp-use/PSVC-thinking-time-per-prompt.rds")

################################################################################
################################################################################
## ratio between first and second half of the task
## KS statistic
## slope of counts / bin

registerDoMC(6)
bins.statistics <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  p.res <- foreach(p=1:length(unique(psvc.tx$prompt[psvc.tx$te_id==id.n])), .combine = rbind) %dopar% {
    p.n <- unique(psvc.tx$prompt[psvc.tx$te_id==id.n])[p]
    df <- psvc.tx %>% filter(te_id == id.n, prompt == p.n)
    task.duration = ifelse(nchar(p.n)==1,30,12)
    if (nrow(df)>1) {
      first.last.ratio <- sum(df$start<(task.duration/2))/(sum(df$start>=(task.duration/2))+sum(df$start<(task.duration/2)))
      ks <- as.numeric(ks.test(df$start,"runif", alternative = "two.sided")$stat)
      
      if (nrow(df)>2) {
        bins <- seq(0,task.duration,6)
        bins.c <- length(bins)-1
        cc <- df %>% mutate(bin = cut(start, breaks = bins,labels = FALSE),
                            bin = case_when(start>task.duration ~max(bins),is.na(bin)~1,T~bin)) %>%
          select(bin) %>% rbind(data.frame(bin = c(1:length(bins)))) %>%
          group_by(bin) %>% summarise(counts = n()-1)
        bins.slope <- summary(lm(cc$counts~cc$bin))$coef[2,1]
      } else {
        bins.slope = NA
      }
      data.frame(te_id = id.n, prompt = p.n, halfs_ratio=first.last.ratio,
                 ks_statistic = ks, Gini_index = as.numeric(ineq::Gini(df$start)),
                 bins_slope = bins.slope)
    } else{
      return(NULL)
    }
  }
  return(p.res)
}
bins.statistics %>%
  write_rds("data/derivatives/tmp-use/PSVC-bins-statistics-per-prompt.rds")

################################################################################
################################################################################
################################################################################
## burst
## High burstiness (B > 0): Suggests efficient lexical access within semantic/phonemic clusters, 
##  followed by executive processes to switch between clusters
## Low burstiness (B ≈ 0 or negative): May indicate more effortful, word-by-word retrieval 
##  or different cognitive strategies
source("src/utils/calc_temporal_features.R")
psvc.burst <- psvc.tx %>% rownames_to_column("pos") %>% group_by(te_id, prompt) %>% filter(start>0) %>%
  arrange(te_id, prompt, start,.by_group = T) %>%
  mutate(count = n(), pos = as.numeric(pos)) %>% filter(count>1) %>%
  mutate(thinking_time_e = start - lag(end), thinking_time_s = start - lag(start),
         thinking_time = ifelse(thinking_time_e < 0, thinking_time_s, thinking_time_e)) %>%
  select(te_id, prompt, thinking_time) %>% drop_na(thinking_time) %>%
  summarise(n = n(), burst_coefficient = calculate_temporal_features(thinking_time)) %>%
  filter(n>=3) %>% select(-n)
write_rds(psvc.burst, "data/derivatives/tmp-use/PSVC-burst-per-prompt.rds", compress = "gz")

################################################################################
################################################################################
################################################################################
## insight patterns
##  looking for long pauses followed by short pauses
source("src/utils/calc_insight_patterns.R")
insight.pattern <- foreach(id=1:length(unique(psvc.tx$te_id)), .combine = rbind) %dopar% {
  id.n <- unique(psvc.tx$te_id)[id]
  pr <- unique(psvc.tx$prompt)
  
  p.res <- foreach(p=1:length(pr), .combine = rbind) %dopar% {
    p.n <- pr[p]
    df <- psvc.tx %>% filter(te_id == id.n, prompt == p.n)
    if (sum(!is.na(df$start))>3) {
      ins.res <- analyze_insight_patterns(response_times = df$start,words = df$response)
      
      data.frame(te_id = id.n, prompt = p.n, 
                 insight_pattern_density = ins.res[[1]],
                 insight_pattern_count = ins.res[[2]],
                 pattern_regularity = ins.res[[3]])
    } else{
      return(NULL)
    }
  }
  
  return(rbind(data.frame(te_id = id.n, prompt = "all", insight_pattern_density = mean(p.res$insight_pattern_density,na.rm=T),
                          insight_pattern_count = sum(p.res$insight_pattern_count,na.rm=T),pattern_regularity = mean(p.res$pattern_regularity,na.rm=T)),
               p.res))
}

insight.pattern %>%
  write_rds("data/derivatives/tmp-use/PSVC-insight-patterns-per-prompt.rds")

################################################################################
################################################################################