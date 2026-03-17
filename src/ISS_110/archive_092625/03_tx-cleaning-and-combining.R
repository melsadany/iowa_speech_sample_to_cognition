################################################################################
################################################################################
rm(list = ls()); gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/workbench/customized-functions/correct_path.R"))
source(correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language")
setwd(project.dir)
################################################################################
################################################################################
pload("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")
ann2 <- ann[8:100,] %>% select(start=1,end=2,5:8) %>% mutate(start = (start/44100), end = (end/44100)) %>%
  mutate(end = ifelse(task=="END", 500,end)) %>% as.data.table()
################################################################################
################################################################################
################################################################################
all.tx.raw.o <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx", sheet = "ISS_transcription_110B")

samples <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx", sheet = "ISS_det") %>%
  select(devGenes_id, te_id, ISS, task_version) %>%
  filter(ISS=="T", task_version=="1.10B") %>%
  rowwise() %>%
  mutate(file_path = paste0("data/derivatives/ISS_transcription/110B/", te_id, ".tsv")) %>% 
  mutate(processed = file.exists(file_path)) %>%
  filter(!te_id %in% unique(all.tx.raw.o$te_id))
################################################################################
################################################################################
patterns <- c(str_split("say check as quickly as possible"," "),
              str_split("thank you for donating your words"," "),
              str_split("read the words on the screen as clearly as you can try to keep up"," "),
              str_split("listen to the sentence when the screen changes repeat the sentence back as best as you can", " "),
              str_split("what was the first word you said in that", " "),
              str_split("for each prompt say as many words as you can think of that fit the prompt", " "),
              str_split("each time you hear hi say hi as quickly as possible", " "),
              str_split("iowa speech sample version", " "))

registerDoMC(cores = 10)
all.tx.raw <- foreach(ii = 1:nrow(samples), .combine = rbind) %dopar% {
  df <- read_tsv(samples$file_path[ii]) %>%
    mutate(word = str_to_lower(word),
           word = gsub("[^[:alnum:]_]","",word))
  
  #### adding annotation here
  iss.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[8]]),.before = 0, .after = length(patterns[[8]])-1,.complete = T)))
  start.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[1]]),.before = 0, .after = length(patterns[[1]])-1,.complete = T)))
  end.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[2]]),.before = 0, .after = length(patterns[[2]])-1,.complete = T)))
  wr.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[3]]),.before = 0, .after = length(patterns[[3]])-1,.complete = T)))
  srt.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[4]]),.before = 0, .after = length(patterns[[4]])-1,.complete = T)))
  wmem.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[5]]),.before = 0, .after = length(patterns[[5]])-1,.complete = T)))
  wa.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[6]]),.before = 0, .after = length(patterns[[6]])-1,.complete = T)))
  hi.idx <- which(do.call(c,slider::slide_index(.x = df$word, .i = seq_along(df$word),.f = ~ all(.x == patterns[[7]]),.before = 0, .after = length(patterns[[7]])-1,.complete = T)))
  
  all.idx <- do.call(c,lapply(list(iss.idx,start.idx,end.idx,wr.idx,srt.idx,wmem.idx,wa.idx,hi.idx), function(x) ifelse(length(x)<1,-999,x)))
  
  df2 <- df %>% 
    mutate_at(.vars = vars(c(start,end)), .funs = function(x) x-df$start[all.idx[1]]) %>%
    mutate(idx = c(1:nrow(df))) %>% relocate(idx) %>%
    filter(idx>(start.idx+length(patterns[[1]])-2)) %>%
    mutate(pred_task = case_when(idx %in% c(all.idx[3]:(all.idx[3]+length(patterns[[2]])-1)) ~ "END",
                                 idx %in% c(all.idx[8]:(all.idx[8]+length(patterns[[7]])-1)) ~ "Hi",
                                 idx %in% c(all.idx[4]:(all.idx[4]+length(patterns[[3]])-1)) ~ "word_reading",
                                 idx %in% c(all.idx[5]:(all.idx[5]+length(patterns[[4]])-1)) ~ "SRT",
                                 idx %in% do.call(c,lapply(wmem.idx, function(x) c(x:(x+length(patterns[[5]]))))) ~ "wmem",
                                 idx %in% c(all.idx[7]:(all.idx[7]+length(patterns[[6]])+8)) ~ "word_association")) %>%
    mutate_at(.vars = vars(c(start,end)), .funs = function(x) replace_na(x, 0)) %>%
    as.data.table()
  
  # ## find the first row that says IOWA SPEECH SAMPLE VERSION .....
  setkey(ann2, start,end);setkey(df2, start,end)
  # match both start and end of tx to the task
  df3 <- foverlaps(x = df2[,.(start,end,word,pred_task)],
                   y = ann2[,.(start,end,task,prompt,type,subtype)],
                   by.x = c("start","end"),
                   type = "within",
                   nomatch = NA) %>%
    select(-c(start,end)) %>% 
    select(start = `i.start`, end = `i.end`, word, pred_task, task, prompt, type, subtype) %>%
    mutate(pred_task = ifelse(is.na(pred_task), prompt, pred_task),
           te_id = samples$te_id[ii])
  
  return(df3)
}
all.tx.raw.2 <- all.tx.raw %>% mutate(subtype=as.character(subtype)) %>%
  mutate_at(.vars = -c(1:2), .funs = function(x) replace_na(x,replace = "")) %>%
  mutate(word_revised = "",wmem_correct = "",comment="") %>%
  select(te_id,start,end,word,word_revised,pred_task,wmem_correct,comment)
write_csv(all.tx.raw.2, "data/derivatives/ISS_transcription/110B-tx-raw.csv")
################################################################################
################################################################################
clean.tx <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx", sheet = "ISS_transcription_110B")
################################################################################
################################################################################
################################################################################
## count comments said
comments <- clean.tx %>%
  filter(comment=="T") %>%
  group_by(te_id) %>%
  dplyr::summarise(comment_count = n())

## clean tx and save
clean.tx.2 <- clean.tx %>%
  filter(is.na(word_revised)|word_revised!="F") %>%
  mutate(word = case_when(is.na(word_revised) ~ word,
                          !is.na(word_revised) ~ word_revised)) %>%
  drop_na(word) %>%
  mutate(word = tolower(word)) %>%
  filter(!word %in% c("uh", "um", "umm", "oh", "eh", "hmm", "hmmm", "ugh", "yeah")) %>% # drop the uh/hmm/um from text analysis
  select(te_id, pred_task, word, start, end)
write_csv(clean.tx.2, "data/derivatives/ISS_transcription/110B-tx-clean.csv")
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
################################################################################
################################################################################
