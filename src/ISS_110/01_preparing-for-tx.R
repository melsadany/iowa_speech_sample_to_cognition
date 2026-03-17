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
samples <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "ISS_det") %>%
  select(devGenes_id, te_id, ISS, task_version) %>%
  filter(ISS=="T", task_version=="1.10B") %>%
  rowwise() %>%
  mutate(file_path = list.files(full.names = T, paste0("/Dedicated/jmichaelson-sdata/MRI/RPOE/", te_id, "/phenotype/speech_sample/"))[1],
         ex = file.exists(file_path)) %>% filter(ex==T) %>% select(-ex) %>%
  mutate(processed = file.exists(paste0("data/derivatives/ISS_transcription/110B/", te_id, ".tsv")))
################################################################################
################################################################################
## prepare whisper tx script if not processed
all.files <- samples %>% dplyr::filter(processed==F)
lines <- c()
## whisperX
if (nrow(all.files)>0) {
  for(i in 1:nrow(all.files)) {
    cmd <- paste("python", "/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/src/utils/transcribe-whisperx-timestamped.py",
                 all.files$file_path[i],
                 paste0(project.dir, "/data/derivatives/ISS_transcription/110B/", all.files$te_id[i], ".tsv"),
                 sep = " ")
    # print(cmd)
    lines <- c(lines, cmd)
  }
  write_lines(c("#!/bin/bash", lines), "src/ISS/02_transcribe.sh")
  # login to argon, open a qlogin with 128 on JM-GPU, activate whisperx conda environment, and run this
  "bash /Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/src/ISS/02_transcribe.sh"
}
################################################################################
################################################################################