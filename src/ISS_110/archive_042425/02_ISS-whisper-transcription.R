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
participants.metadata <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx", 
                                           sheet = "ISS_det")
################################################################################
################################################################################
# make your list of participants and their cropped audio files
p.files <- list.dirs("data/derivatives/ISS_participants-response", recursive = F, full.names = F)
all.files.r <- data.frame(te_id = rep(p.files, each = 3),
                          part = rep(c(1:3), length(p.files))) %>%
  mutate(file = paste0(project.dir, "/data/derivatives/ISS_participants-response/",
                       te_id, "/", te_id, "_ISS-pt", part, ".wav"))
######
# if running for a new/specific participant
pid <- c(paste0("2E_0", c(77,78,92,95:99)), paste0("2E_", c(100:109,112)))
all.files <- all.files.r %>%
  filter(te_id %in% pid)
######
# run whisper on these files. whisper runs only on cropped tasks, not the full audio
lines <- c()
for(i in 1:nrow(all.files)) {
  n.dir <- paste0(project.dir, "/", "data/derivatives/ISS_transcription/", all.files$te_id[i])
  system(paste0("rm ", n.dir))
  system(paste0("mkdir -p ", n.dir))
  cmd <- paste("whisper_timestamped",
               all.files$file[i], 
               "--model large-v3",
               "--language en",
               "--accurate",
               "--punctuations_with_words False",
               "--verbose True",
               "--detect_disfluencies True",
               "--output_format tsv",
               "--threads 125",
               "--compute_confidence True",
               "--output_dir", n.dir, 
               sep = " ")
  # print(cmd)
  # system(cmd)
  lines <- c(lines, cmd)
}
write_lines(c("#!/bin/bash", lines), "src/ISS/02_transcribe.sh")
"cat /Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/src/ISS/02_transcribe.sh | parallel -j 35"

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# combine whisper transcription files per participants
whisper.files <- all.files.r %>%
  mutate(file = sub("participants-response", "transcription", sub("\\.wav", ".wav.words.tsv", file)))
registerDoMC(cores = 3)
all.transcriptions <- foreach(i = 1:nrow(whisper.files), .combine = rbind) %dopar% {
  if (file.exists(whisper.files$file[i])) {
    part <- whisper.files$part[i]
    t <- read_tsv(whisper.files$file[i]) %>%
      filter(text != "[*]") %>%
      mutate(te_id = whisper.files$te_id[i],
             start = case_when(part == 1 ~ start,
                               part == 2 ~ start+150000,
                               part == 3 ~ start+300000),
             end = case_when(part == 1 ~ end,
                             part == 2 ~ end+150000,
                             part == 3 ~ end+300000))
    if (part == 1) {
      sstart = 1
      for (iii in 1:200) {
        tt <- paste(tolower(t$text[iii:(iii+length(findd)-1)]), collapse = " ")
        if (("each time the checkbox moves say" == tt)|tt=="each time the check box moves") {
          sstart = iii
        }
      }
      t <- t[sstart:nrow(t),]
    }
    return(t)
    # return(NULL)
  } else {
    return(NULL)
  }
}
participants.start <- all.transcriptions %>%
  group_by(te_id) %>%
  slice_head(n = 1)

# get the task annotations
iss.ann <- (ann %>% select(seconds, task, type, subtype))[7:100,]
iss.ann <- iss.ann[1:93,] %>% 
  rename(start = seconds) %>%
  mutate(end = lead(iss.ann$seconds)[1:93]-0.001) %>%
  relocate(end, .after = start) %>%
  mutate_at(.vars = vars(c(start, end)), .funs = function(x) x-iss.ann$seconds[1]) %>%
  mutate_at(.vars = vars(c(start, end)), .funs = function(x) x*1000)

# set the task start as 0 time for each participant
tx <- left_join(all.transcriptions, 
                participants.metadata %>% select(te_id, task_version)) %>%
  filter(task_version == "1.10B") %>%
  left_join(participants.start %>% select(te_id, task_start = start)) %>%
  mutate(raw_start = start,
         raw_end = end,
         start = start - task_start,
         end = end - task_start) %>%
  select(-task_start, -task_version)
# include the task information based on the response timing
tx2 <- tx %>%
  rowwise() %>%
  mutate(task = iss.ann$task[which(start >= iss.ann$start & end <= iss.ann$end)[1]],
         type = iss.ann$type[which(start >= iss.ann$start & end <= iss.ann$end)[1]],
         subtype = iss.ann$subtype[which(start >= iss.ann$start & end <= iss.ann$end)[1]]) %>%
  relocate(raw_start, raw_end)
table(is.na(tx2$task))

write_tsv(tx2, "data/derivatives/ISS_transcription/v110B-tx-raw.tsv")
################################################################################
################################################################################
################################################################################
# after saving the files combined, you should manually revise the transcription
# read the revised version and keep it for downstream analysis
################################################################################
################################################################################


################################################################################


################################################################################