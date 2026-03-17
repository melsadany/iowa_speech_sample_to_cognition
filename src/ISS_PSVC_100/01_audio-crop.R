################################################################################
################################################################################
rm(list = ls());gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(tuneR)
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis"
setwd(project.dir)
################################################################################
################################################################################
# read the PS_VC task metadata
ps.vc.metadata.r <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "PS-VC_task_metadata") %>%
  filter(task_v==2)
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec)

# keep participants of interest
participants.metadata <- readxl::read_excel("data/RPOE_meta.xlsx",sheet = "PS-VC_participants-metadata")
p.of.int <- participants.metadata %>% 
  mutate(duration = format(as.POSIXct(duration), format = "%H:%M:%S"),
         first_beep = format(as.POSIXct(first_beep), format = "%H:%M:%S"),
         start = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", first_beep)),
         duration_m = as.numeric(sub("^([^:]+).*", "\\1", duration)),
         duration_s = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", duration)),
         duration_in_sec = (60*duration_m)+duration_s
  ) %>%
  select(te_id, starts_with("duration"), start, audio, video, `all tasks completed`)
################################################################################
################################################################################
# copy ps-vc file to my dir, make the mp3 file, and crop per task
data.dir <- "/Dedicated/jmichaelson-sdata/MRI/RPOE"
my.data.dir <- paste0(project.dir, "/data/raw/PS_vc")
for (i in 1:nrow(p.of.int)) {
  pid <- p.of.int$te_id[i]
  raw.a.file <- gsub(" ", fixed = T, "\\ ", 
                     list.files(paste0(data.dir, "/", pid, "/phenotype/PS_verbal_screen"),
                                pattern = ".mp3", full.names = T))
  a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  if (!file.exists(a.file) && length(raw.a.file) == 0) { # make mp3 file if the raw is only video
    v.file <- gsub(" ", fixed = T, "\\ ", 
                   list.files(paste0(data.dir, "/", pid, "/phenotype/PS_verbal_screen"),
                              full.names = T, pattern = ".mp4")[1])
    system(paste0("ffmpeg -i ", v.file, " -ar 44100 -ac 2 ", 
                  my.data.dir, "/", pid, ".mp3"))
    a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  }else if(!file.exists(a.file)) { # copy the mp3 file, if it doesn't exist in my dir
    system(str_c("cp ", raw.a.file," ", a.file))
  }
  # read the full mp3, and crop the task only
  aud <- readMP3(a.file)
  aud.cropped <- extractWave(aud, xunit = "time", from = p.of.int$start[i], 
                             to = p.of.int$duration_in_sec[i])
  # save full task response
  system(paste0("mkdir -p ",project.dir,"/data/derivatives/PS-VC_participants-response/",pid))
  writeWave(Wave(left = as.numeric(aud.cropped@left),samp.rate = 44100, bit = 16, pcm=T), 
            filename = paste0(project.dir, "/data/derivatives/PS-VC_participants-response/",
                              pid, "/", "full-PS_VC-cropped_",pid, ".wav"), 
            extensible = T)
  # get responses cropped
  for (j in 1:nrow(ps.vc.metadata)) {
    task <- paste0(pid,"_task-", ps.vc.metadata$task_num[j], 
                   "_", ps.vc.metadata$word[j], "_", j)
    t.aud <- extractWave(aud.cropped, from = ps.vc.metadata$start_in_sec[j], 
                         to = ps.vc.metadata$end_in_sec[j], xunit = "time")
    # saving the task output
    writeWave(Wave(left = as.numeric(t.aud@left),samp.rate = 44100,bit = 16,pcm=T), 
              filename = paste0(project.dir,"/data/derivatives/PS-VC_participants-response/",
                                pid, "/", task, ".wav"), 
              extensible = T)
  }
}
################################################################################
################################################################################