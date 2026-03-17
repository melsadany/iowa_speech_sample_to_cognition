################################################################################
################################################################################
rm(list = ls());gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
#  read the PS_VC task metadata
ps.vc.metadata.r <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "PS-VC_task_metadata") %>%
  filter(task_v==2)
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec)
################################################################################
################################################################################
# make your list of participants and their cropped audio files
p.files <- data.frame(file = list.dirs("data/derivatives/PS-VC_participants-response", 
                                       recursive = F, full.names = T))
all.files <- data.frame(file = list.files(p.files$file, pattern = "task", full.names = T)) %>%
  mutate(task = sub("_.*", "", sub(".*task-", "", basename(file))),
         word = sub("_.*", "", sub(".*task-[0-9]_", "", basename(file))),
         ID = sub("_task.*", "", basename(file)))
ran.files <- all.files %>% filter(word %in% c("5b5","6b6"))

# run whisper on these files. whisper runs only on cropped tasks, not the full audio
registerDoMC(cores = 6)
cmds <- foreach(i = 1:nrow(all.files), .combine = rbind) %dopar% {
  n.dir <- paste0(project.dir, "/", "data/derivatives/PS-VC_transcription/", all.files$ID[i])
  system(paste0("mkdir -p ", n.dir))
  cmd <- paste("whisper_timestamped",
               paste0(project.dir, "/", all.files$file[i]), 
               "--model large-v3",
               "--language en",
               "--accurate",
               "--punctuations_with_words False",
               "--verbose True",
               "--detect_disfluencies True",
               "--output_format tsv",
               "--threads 1",
               "--output_dir", n.dir, 
               sep = " ")
  return(cmd)
}
# save the file paths to run whisper on argon bash for loop?
write_lines(rbind("#!/bin/bash",cmds), "src/02_02_whisper-run.sh")

# then, you'd go to a terminal and run the following:
paste0("EDL; cat ", project.dir, "/src/02_02_whisper-run.sh | parallel -j 35")

################################################################################
################################################################################
# combine whisper transcription files per participants
whisper.files <- all.files %>% mutate(ID = sub("_task.*", "", basename(file))) %>%
  mutate(file = sub("participants-response", "transcription", sub("\\.wav", ".wav.words.tsv", file)))
registerDoMC(cores = 3)
all.transcriptions <- foreach(i = 1:nrow(whisper.files), .combine = rbind) %dopar% {
  t <- read_tsv(whisper.files$file[i]) %>%
    mutate(task = whisper.files$task[i],
           word = whisper.files$word[i],
           ID = whisper.files$ID[i])
  return(t)
}

t13.transcriptions <- left_join(all.transcriptions, 
                                ps.vc.metadata %>% mutate(task_order = c(1:nrow(ps.vc.metadata))) %>% select(-task_num), 
                                relationship = "many-to-many") %>%
  filter(task %in% c("1","3")) %>%
  mutate(text = str_replace_all(text, "\\.", ""),
         text = str_replace_all(text, "\\.\\.\\.", ""),
         text = tolower(text))  %>% # Remove ...
  filter(text != "[*]", text != ".", nchar(text) > 0) %>%
  group_by(ID, task_order) %>%
  arrange(.by_group = T)
t24.transcriptions <- left_join(all.transcriptions, 
                                ps.vc.metadata %>% mutate(task_order = c(1:nrow(ps.vc.metadata))) %>% select(-task_num), 
                                relationship = "many-to-many") %>%
  filter(task %in% c("2","4"))  %>%
  group_by(ID, task_order) %>%
  arrange(.by_group = T)
# save combined transcriptions
write_tsv(t13.transcriptions, "data/derivatives/PS-VC_transcription/task-1-and-3-all-together-whisper-transcription-raw-timestamped.tsv")
write_tsv(t24.transcriptions, "data/derivatives/PS-VC_transcription/task-2-and-4-all-together-whisper-transcription-raw-timestamped.tsv")

################################################################################
################################################################################
# after saving the files combined, you should manually revise the transcription
# read the revised version and keep it for downstream analysis

################################################################################
################################################################################
## whisperX for RAN
registerDoMC(cores = 6)
cmds.ran <- foreach(i = 1:nrow(ran.files), .combine = rbind) %dopar% {
  n.dir <- paste0(project.dir, "/", "data/derivatives/PS-VC_transcription/", ran.files$ID[i])
  system(paste0("mkdir -p ", n.dir))
  cmd <- paste("python", file.path(project.dir,"src/utils/transcribe-whisperx-timestamped.py"),
               paste0(project.dir, "/", ran.files$file[i]), 
               paste0(n.dir,"/",ran.files$ID[i],sub("\\.wav","",sub(".*_task-2","",ran.files$file[i])),".tsv"), 
               sep = " ")
  return(cmd)
}
write_lines(rbind("#!/bin/bash",cmds.ran), "src/whisperx-RAN-run.sh")
# then, you'd go to a terminal and run the following:
paste0("EDL; cat ", project.dir, "/src/whisperx-RAN-run.sh | parallel -j 10")

################################################################################
################################################################################
ran.files.2 <- data.frame(file = list.files(sub("participants-response","transcription",p.files$file), pattern = "task-2", full.names = T)) %>%
  filter(grepl(".wav.words.tsv",file)) %>%
  mutate(te_id = sub("/.*","",sub(".*transcription/","",file)),
         prompt = sub("\\.wav.*","",sub(".*task-2_","",file)))

ran.transcriptions <- foreach(i = 1:nrow(ran.files.2), .combine = rbind) %dopar% {
  if (!file.exists(ran.files.2$file[i])) {
    return(NULL)
  }
  t <- read_tsv(ran.files.2$file[i]) %>%
    mutate(prompt = ran.files.2$prompt[i],
           te_id = ran.files.2$te_id[i])
  return(t)
}
ran.durations <- ran.transcriptions %>% 
  mutate(text=tolower(text),
         chr = grepl("[a-z,A-Z]",text),silence=grepl("\\*",text),
         num = grepl(paste(c(0:10,c("zero","one","two","three","four","five","six","seven","eight","nine","ten")),
                           collapse = "|"),text),
         drop = (chr&!num|silence)) %>%filter(!drop) %>%
  group_by(te_id, prompt) %>% summarise(start = min(start), end = max(end)) %>%
  ungroup() %>% mutate(duration = (end/1000)-(start/1000)) %>% filter(duration>1) %>%
  mutate(dim=sub("_.*","",prompt)) %>% group_by(te_id,dim) %>% summarise(duration = mean(duration))
write_csv(ran.durations, "data/derivatives/RAN-durations.csv")

################################################################################
################################################################################
faces.files <- data.frame(file = list.files(sub("participants-response","transcription",p.files$file), pattern = "task-4", full.names = T)) %>%
  filter(grepl(".wav.words.tsv",file)) %>%
  mutate(te_id = sub("/.*","",sub(".*transcription/","",file)),
         prompt = sub("\\.wav.*","",sub(".*task-4_","",file)))

faces.transcriptions <- foreach(i = 1:nrow(faces.files), .combine = rbind) %dopar% {
  if (!file.exists(faces.files$file[i])) {
    return(NULL)
  }
  t <- read_tsv(faces.files$file[i]) %>%
    mutate(prompt = faces.files$prompt[i],
           te_id = faces.files$te_id[i])
  return(t)
}
faces.durations <- faces.transcriptions %>% 
  mutate(text=tolower(text),
         chr = grepl("[a-z,A-Z]",text),silence=grepl("\\*",text),
         num = grepl(paste(c(0:10,c("zero","one","two","three","four","five","six","seven","eight","nine","ten")),
                           collapse = "|"),text),
         drop = (chr&!num|silence)) %>%filter(!drop) %>%
  group_by(te_id, prompt) %>% summarise(start = min(start), end = max(end)) %>%
  ungroup() %>% mutate(duration = (end/1000)-(start/1000)) %>% filter(duration>1) %>%
  group_by(te_id) %>% summarise(duration = mean(duration))
write_csv(faces.durations, "data/derivatives/faces-durations.csv")

################################################################################
################################################################################
## whisperX for the entire task
p.meta <- data.frame(file = list.files("data/derivatives/PS-VC_participants-response/", pattern = "full-PS_VC", recursive = T)) %>%
  mutate(te_id = sub("/.*","",file),
         full_path = paste0(project.dir, "/data/derivatives/PS-VC_participants-response/",
                            file),
         out = paste0(project.dir, "/data/derivatives/PS-VC_transcription/", te_id,
                      "/full-task-whisperX.tsv"))
write_csv(p.meta %>% select(full_path, out), "data/raw/whisperx-files.csv")
registerDoMC(cores = 6)
cmds.full <- foreach(i = 1:nrow(p.meta), .combine = rbind) %dopar% {
  system(paste0("mkdir -p ", project.dir, "/data/derivatives/PS-VC_transcription/", p.meta$te_id[i]))
  cmd <- paste("python", file.path(project.dir,"src/utils/transcribe-whisperx-timestamped.py"),
               p.meta$full_path[i], 
               p.meta$out[i], 
               sep = " ")
  return(cmd)
}
write_lines(rbind("#!/bin/bash",cmds.full), "src/02_whisperx-run.sh")
# then, you'd go to a terminal and run the following:
paste0("EDL; cat ", project.dir, "/src/02-whisperx-run.sh | parallel -j 4")

## combine responses and map to prompts
psvc.meta <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "PS-VC_participants-metadata") %>%
  select(te_id, first_beep, done = PS_VC,version=`PS-VC_version`)

psvc.design.1 <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "PS-VC_task_metadata") %>% 
  filter(task_v==1) %>% 
  select(prompt = word, start_in_sec, end_in_sec) %>%
  mutate(start = start_in_sec-min(start_in_sec),
         end = end_in_sec-min(start_in_sec)) %>% select(-c(start_in_sec, end_in_sec))
psvc.design.2 <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "PS-VC_task_metadata") %>% 
  filter(task_v==2) %>% 
  select(prompt = word, start_in_sec, end_in_sec) %>%
  mutate(start = start_in_sec-min(start_in_sec),
         end = end_in_sec-min(start_in_sec)) %>% select(-c(start_in_sec, end_in_sec))

## combine tx
tx.all <- do.call(rbind, lapply(c(1:nrow(p.meta)), function(x) {
  read_tsv(p.meta$out[x]) %>% mutate(te_id = p.meta$te_id[x])}))
tx.2 <- psvc.meta %>% filter(done=="T",version==2) %>% select(te_id) %>% left_join(tx.all)
tx.2.clean = fuzzyjoin::interval_left_join(tx.2 %>% drop_na(), psvc.design.2,
                                           c("start" = "start",
                                             "end" = "end"))

tx.clean.all <- tx.2.clean %>%
  mutate(`end.x`=`end.x`-`start.y`,`start.x`=`start.x`-`start.y`) %>%
  select(1,start=2,end=3,prompt,response=word)

write_csv(tx.clean.all,"data/derivatives/PS-VC_transcription/whisperx-tx-full-audio.csv")

################################################################################
################################################################################
all <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "PS-VC_transcription-revision") %>%
  select(te_id=ID, prompt = word, response = text, text_revised, start_o=start, end_o=end,comment) %>% 
  mutate(prompt = tolower(prompt), response = tolower(response), text_revised,
         start_o=start_o/1000,end_o=end_o/1000)
all.clean <- read_rds("data/derivatives/ps-vc-text-clean.rds") %>%
  mutate(prompt = tolower(word), response = tolower(text), start_o = start/1000, end_o = end/1000) %>%
  select(te_id, prompt, response, start_o, end_o)
tttmp <- full_join(tx.clean.all %>% mutate(response = str_replace_all(tolower(response), ",|\\.",""), 
                                           prompt = tolower(prompt)), 
                   all) %>%
  filter(prompt %in% psvc.design.1$prompt[c(1:20,25:29)]) %>%
  mutate(prompt=factor(prompt, levels = psvc.design.1$prompt[c(1:20,25:29)])) %>%
  arrange(te_id,prompt)

tttmp3 <- full_join(tx.clean.all %>% mutate(response = str_replace_all(tolower(response), ",|\\.",""), 
                                            prompt = tolower(prompt)), 
                    all.clean) %>%
  filter(prompt %in% tolower(psvc.design.1$prompt[c(25:29)])) %>%
  mutate(prompt=factor(prompt, levels = tolower(psvc.design.1$prompt[c(25:29)]))) %>%
  arrange(te_id,prompt)

write_csv(tttmp,"data/derivatives/PS-VC_transcription/whisperx-tx-full-audio_v2.csv")
write_csv(tttmp3,"data/derivatives/PS-VC_transcription/whisperx-tx-full-audio_v22.csv")

################################################################################
################################################################################
################################################################################