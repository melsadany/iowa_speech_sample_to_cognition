################################################################################
#                           extraction of ISS features                         #
################################################################################
rm(list = ls())
gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
                      "/jmichaelson-wdata/msmuhammad/projects/RPOE/language")
################################################################################
################################################################################
# load files here
v.1.04.tx <- read_tsv("data/derivatives/ISS_transcription/v104-tx-raw.tsv")
v.1.10.tx <- read_tsv("data/derivatives/ISS_transcription/v110B-tx-raw.tsv")

iss.meta <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx", 
                              sheet = "ISS-metadata")
iss.meta.1.04 <- iss.meta %>%
  filter(task_v == "1.04A") %>%
  mutate(start_in_sec = start_in_sec-29,
         end_in_sec = end_in_sec-29)
iss.meta.1.10 <- iss.meta %>%
  filter(task_v == "1.10B") %>%
  mutate(start_in_sec = start_in_sec-34,
         end_in_sec = end_in_sec-34)

################################################################################
################################################################################
# crop tx per task

# v.1.04
v.1.04.2 <- fuzzyjoin::fuzzy_left_join(v.1.04.tx, 
                                       iss.meta.1.04 %>% 
                                         mutate(start_in_sec = start_in_sec*1000,
                                                end_in_sec = end_in_sec * 1000) %>%
                                         select(1,2,task_v,start_in_sec, end_in_sec),
                                       by = c("start" = "start_in_sec", "end" = "end_in_sec"),
                                       match_fun = list(`>=`, `<=`)) %>%
  select(-ends_with("in_sec"))
v.1.10.2 <- fuzzyjoin::fuzzy_left_join(v.1.10.tx, 
                                       iss.meta.1.10 %>% 
                                         mutate(start_in_sec = start_in_sec*1000,
                                                end_in_sec = end_in_sec * 1000) %>%
                                         select(1,2,task_v,start_in_sec, end_in_sec),
                                       by = c("start" = "start_in_sec", "end" = "end_in_sec"),
                                       match_fun = list(`>=`, `<=`)) %>%
  select(-ends_with("in_sec"))

all.r <- rbind(v.1.04.2, v.1.10.2) %>% 
  mutate(text = tolower(text)) %>%
  filter(text != "[*]") %>%
  rownames_to_column("row_num") %>%
  mutate(row_num = as.numeric(row_num))
write_csv(all.r, "data/derivatives/ISS_transcription/ISS-all-mapped.csv")
# after this, you'll need to manually QC the transcriptions

################################################################################
################################################################################
################################################################################
################################################################################
# QC Notes:
#     1. 2E_052 didn't say breathtaking
#     2. tx for some participants was terrible and highlighted in red
#     3. I dropped 2E_076; no good responses

# clean the tx here, and save
all.r <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx",
                           sheet = "ISS_transcription")
all <- all.r %>%
  filter(text_revised != "F" | is.na(text_revised)) %>%
  mutate(text = ifelse(is.na(text_revised), text, text_revised),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  select(-c(text_revised, word_revised, task_num))

################################################################################
################################################################################
################################################################################
################################################################################
# task #1 Checkbox

# Find the "check" words
p.cb <- all %>%
  mutate(next_lead = lead(text)) %>%
  filter(word == "CB") %>%
  filter(text == "check" & next_lead == "check" |
           text == "check" & next_lead == "each")

# get the timepoint of the task
cb.start.point <- all %>%
  mutate(next_lead = lead(text)) %>%
  filter(text == "possible" & next_lead == "check") %>%
  select(te_id, trigger = end)
# subtract response time - trigger time
p.cb.2 <- p.cb %>% left_join(cb.start.point) %>%
  mutate(response_start = start-trigger) %>%
  select(2:5, 8:10)

cb.summary <- p.cb.2 %>%
  group_by(te_id) %>%
  dplyr::summarise(avg_CB_response_time = mean(response_start))

################################################################################
################################################################################
# task #2 Hi

# Find the "Hi" words
p.hi <- all %>%
  mutate(next_lead = lead(text)) %>%
  filter(text == "hi" & next_lead == "hi" |
           text == "hi" & next_lead == "for") %>%
  select(2:5) %>%
  group_by(te_id) %>%
  mutate(count = 1:n()) %>% ungroup() %>%
  mutate(source = ifelse(count %in% c(1,3,5,7,9), "task", "response"),
         set = case_when(count %in% c(1,2) ~ "A",
                         count %in% c(3,4) ~ "B",
                         count %in% c(5,6) ~ "C",
                         count %in% c(7,8) ~ "D",
                         count %in% c(9,10) ~ "E"),
         time = ifelse(source == "task", end, start)) %>%
  pivot_wider(names_from = "source", values_from = "time", id_cols = c(te_id, set)) %>%
  mutate(response_delay = response-task) 

hi.summary <- p.hi %>%
  group_by(te_id) %>%
  dplyr::summarise(avg_hi_response_time = mean(response_delay, na.rm = T))

################################################################################
################################################################################
# word reading
wr.task.words <- data.frame(num = c(1:50),
                            word = c("breathtaking","triumph","masterpiece","exuberant","rejoice",
                                     "miracle","pleasure","admire","joyous","calm",
                                     "outrage","abused","bother","embarrass","ugly",        
                                     "failing","trouble","neglect","uptight","worry",       
                                     "not","trap","saw","stared","emit",        
                                     "edit","tarp","raw","flow","tide",        
                                     "reward","help","fondness","rescued","optimistic",  
                                     "noble","beautiful","inspiring","improve","supportive",  
                                     "protest","terror","restrict","bore","ban",         
                                     "disgraced","alarmed","enraged","chaos","block"))

# this one is a bit tricky because some of the whisper tx output were trash
wr.to.drop <- c(paste0("2E_0", c(51, 59,61, 67,71,77,82,83,85,87,90,91,94,
                                 78,92,96,97,98)),
                paste0("2E_", c(100,107)))

p.wr.start <- all %>%
  filter(!(te_id %in% wr.to.drop),
         word == "word_reading") %>%
  select(-row_num) %>%
  rownames_to_column("row_num") %>%
  mutate(row_num = as.numeric(row_num)) %>%
  group_by(te_id) %>%
  dplyr::summarise(first_word_time = mean(start),
                   start_row_num = max(row_num)) %>%
  drop_na()

wr.correct <- all %>% 
  filter(word == "word_reading") %>% 
  left_join(p.wr.start) %>%
  filter(text %in% wr.task.words$word) %>%
  distinct(te_id, text, .keep_all = T) %>%
  group_by(te_id) %>% summarise(WR_correctly_said = n()) %>%
  drop_na()



all %>% select(1:5) %>%
  left_join(p.wr.start) %>%
  distinct(te_id, text, .keep_all = T) %>%
  filter(row_num >= start_row_num & row_num < (start_row_num+60)) %>%
  group_by(te_id) %>% summarise(c = n()) %>% print(n=50)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Jake's code
library(tuneR)
# functions
##### 
### find audio peaks
find_peaks <- function(vector, thresh, result = 'center') {
  # Calculate the differences
  diffs <- diff(vector)
  
  # Identify where the diffs change from positive to negative, indicating a peak
  peak_indices <- which(diffs[-length(diffs)] > 0 & diffs[-1] < 0) + 1
  peak_indices <- peak_indices[vector[peak_indices] > thresh]
  if(length(peak_indices)==0) return(NA)
  if (result == 'center') {
    return(peak_indices)
  } else if (result == 'onset') {
    onset_indices <- numeric(length(peak_indices))
    for (i in 1:length(peak_indices)) {
      peak_idx <- peak_indices[i]
      # Scan backwards to find the onset
      onset_idx <- peak_idx
      while (onset_idx > 1 && vector[onset_idx - 1] < vector[peak_idx] && vector[onset_idx - 1] > thresh) {
        onset_idx <- onset_idx - 1
      }
      onset_indices[i] <- onset_idx
    }
    return(onset_indices)
  } else {
    stop("Invalid 'result' parameter value. Use 'center' or 'onset'.")
  }
}

### align wav (q1) to template (idx)
align_to_template = function(q1,idx=NULL,t1=NULL,maxlen=21368275){
  s = seq(0,30*44100,1000)
  if(!is.null(t1) & is.null(idx)){
    idx = which(abs(t1@left)>20000)
    idx = (idx-idx[1])[-1]
    idx = idx[c(TRUE,diff(idx) > 10)]
  }
  
  chk = sapply(s,function(x,y) sum(y[idx + x]),abs(q1@left))
  #s[which.max(chk)]
  ### scan +/- 1000 samples of the best coarse match
  sfine = c(-1000:1000) + s[which.max(chk)]
  chkfine = sapply(sfine,function(x,y) sum(y[idx + x]),abs(q1@left))
  best_fine = sfine[which.max(chkfine)]
  interval = best_fine:(best_fine+maxlen)
  print(paste("start estimated at",round(best_fine/44100,2),"seconds"))
  return(q1[interval])
}

### function to get reading times
### q1a is aligned "query" WAV/MP3
### ann is the "annotation" of the template
reading_time = function(q1a,ann,plot.it=TRUE,drop_before=250){
  r = q1a[ann[[1]][50]:ann[[2]][99]]
  r = abs(r@left) + abs(r@right)
  words = rep(1:50,each=44100*1.2)
  ll = tapply(r[1:length(words)],words,function(x) lowess(x,f=1/10)$y[seq(1,52920,length.out=1200)])
  foo = do.call('rbind',ll)
  foo[foo<(median(r,na.rm=T)+mad(r,na.rm=T))] = 0
  front0 = apply(foo,1,function(x) which(x==0)[1])
  front0[is.na(front0)] = 1
  for(i in 1:50){
    foo[i,1:front0[i]] = 0
  }
  rownames(foo) = ann$prompt[50:99]
  foo[,1:(drop_before-1)] = 0
  if(plot.it){
    img(foo,xlab="milliseconds",do.breaks=F)
    abline(h=c(10,20,30,40),col='white')
    points(apply(foo,1,function(x) which(x>0)[1]),(1:50)-0.5,col='white')
    axis(1,at=c(0,300,600,900,1200),labels=c(0,300,600,900,1200))
    mtext(1,at=apply(foo,1,function(x) which(x>0)[1]),text="|",col='grey')
  }
  starts = apply(foo,1,function(x) which(x>0)[1])
  out = c(median=median(starts),mad=mad(starts))
  return(starts)
  return(out)
}


## auditory-oral reaction time
ao_rxn = function(q1a,ann,thresh=2000,plot.it=TRUE){
  r = normalize(q1a[ann[[1]][14]:ann[[2]][18]],unit="16")
  r = abs(r@left) + abs(r@right)
  words = rep(1:5,ann[[1]][15:19]-ann[[1]][14:18])[1:length(r)]
  ll = tapply(r[1:length(words)],words,function(x) approx(lowess(x,f=1/100)$y,xout=seq(1,44100*2,length.out=2000))$y)
  foo = do.call('rbind',ll)
  foo[is.na(foo)] = 0
  #print(range(foo))
  #return(foo)
  peaks = apply(foo,1,function(x) diff(find_peaks(x,thresh=thresh)[1:2]))
  #print(peaks)
  if(sum(is.na(peaks))>1) stop("not enough peaks detected...maybe adjust threshold?")
  out = c(
    median=median(peaks,na.rm=T),
    mad=mad(peaks,na.rm=T)
  )
  if(plot.it){
    img(foo)
    abline(v=out[1]+c(-3*out[2],0,3*out[2]),lty=2,col='white')
  }
  return(out)
}

## visual-oral reaction time
vo_rxn = function(q1a,ann,thresh=2000,plot.it=T){
  r = normalize(q1a[ann[[1]][8]:ann[[2]][12]],unit="16")
  r = abs(r@left) + abs(r@right)
  words = rep(1:5,ann[[1]][9:13]-ann[[1]][8:12])[1:length(r)]
  ll = tapply(r[1:length(words)],words,function(x) approx(lowess(x,f=1/100,delta=100)$y,xout=seq(1,44100*2,length.out=2000))$y)
  foo = do.call('rbind',ll)
  foo[is.na(foo)] = 0
  peaks = apply(foo,1,function(x) find_peaks(x,thresh=thresh,result="onset")[1])
  if(sum(is.na(peaks))>1) stop("not enough peaks detected...maybe adjust threshold?")
  # print(peaks)
  out = c(
    median=median(peaks,na.rm=T),
    mad=mad(peaks,na.rm=T)
  )
  if(plot.it){
    img(foo)
    abline(v=out[1]+c(-3*out[2],0,3*out[2]),lty=2,col='white')
  }
  return(out)
}
#####

pload("src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")

ff = list.files("/wdata/msmuhammad/projects/RPOE/language/data/raw/ISS",full.names=T,pattern="mp3")
qs = lapply(ff,readMP3)
qsa = lapply(qs,align_to_template,idx)[c(35,49,51:66)] ### I only know these to be v1.10

visual = sapply(c(5000,2000,1000,500,200),function(y) 
  as.numeric(sapply(qsa,function(x) try(vo_rxn(x,ann,y,F),silent=F)[1])))
auditory = sapply(c(5000,2000,1000,500,200),function(y) 
  as.numeric(sapply(qsa,function(x) try(ao_rxn(x,ann,y,F),silent=F)[1])))
reading = sapply(c(250,300),function(y) 
  as.numeric(sapply(qsa,function(x) try(reading_time(x,ann,y,F),silent=F)[1])))


plot(auditory[,4],visual[,2],ylim=c(0,600),xlim=c(0,600),ylab="visual-vocal RT (median ms)",
     xlab="auditory-vocal RT (median ms)")
abline(0,1,col='grey')
abline(lm(visual[,2]~auditory[,4]),col='orangered',lty=2)



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# this section will need to be cut to a new script
m1m2 <- read_rds("../shared_data/data/m1m2.rds")
demo <- read_csv("../shared_data/data/demo-full.csv")

iss.summ <- inner_join(m1m2, demo) %>%
  left_join(cb.summary) %>%
  left_join(wr.correct) %>%
  left_join(hi.summary)

# NIH-TB
iss.summ %>%
  pivot_longer(cols = colnames(m1m2)[c(3:14)], names_to = "IQ_task", values_to = "IQ_score") %>%
  mutate(IQ_task = sub("_age_corrected_standard_score", "", IQ_task),
         IQ_task = sub("composite", "", IQ_task)) %>%
  pivot_longer(cols = c(avg_hi_response_time, avg_CB_response_time, WR_correctly_said),
               names_to = "V2", values_to = "V2_val") %>%
  ggplot(aes(x=V2_val, y = IQ_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red", size = 2, method = "spearman") +
  ggh4x::facet_grid2(rows = vars(IQ_task), cols = vars(V2), scales = "free", independent = T)
ggsave("figs/ISS-summary-1.png", bg = "white",
       width = 8, height = 18, units = "in", dpi = 360)
# IQ
iss.summ %>%
  pivot_longer(cols = colnames(m1m2)[c(15:28)], names_to = "IQ_task", values_to = "IQ_score") %>%
  pivot_longer(cols = c(avg_hi_response_time, avg_CB_response_time, WR_correctly_said),
               names_to = "V2", values_to = "V2_val") %>%
  ggplot(aes(x=V2_val, y = IQ_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red", size = 2, method = "spearman") +
  ggh4x::facet_grid2(rows = vars(IQ_task), cols = vars(V2), scales = "free", independent = T)
ggsave("figs/ISS-summary-2.png", bg = "white",
       width = 8, height = 18, units = "in", dpi = 360)
# binary
iss.summ %>%
  mutate(sex_M = ifelse(sex == "M", T, F)) %>%
  pivot_longer(cols = c(ADHD_dx, ASD_dx, sex_M), names_to = "demo", values_to = "demo_val") %>%
  pivot_longer(cols = c(avg_hi_response_time, avg_CB_response_time, WR_correctly_said),
               names_to = "V2", values_to = "V2_val") %>%
  ggplot(aes(y=V2_val, x = demo_val, fill = demo_val)) +
  geom_violin() + geom_boxplot(fill = "white", width = 0.2) +
  ggpubr::stat_compare_means(color = "red", size = 3) +
  ggh4x::facet_grid2(rows = vars(V2), cols = vars(demo), scales = "free", independent = T)
ggsave("figs/ISS-summary-3.png", bg = "white",
       width = 8, height = 8, units = "in", dpi = 360)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
