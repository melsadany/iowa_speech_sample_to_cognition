################################################################################
#                 processing audio files from the ISS and cropping             #
################################################################################
rm(list = ls())
gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
              "/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
library(tuneR)
################################################################################
################################################################################
project.dir <- paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),
                      "/jmichaelson-wdata/msmuhammad/projects/RPOE/language")
setwd(project.dir)
################################################################################
################################################################################
################################################################################
################################################################################
# # groundtruth work
# 
# # I realized that the beep sound is different in both task versions
# # so, handle them differently
# 
# 
# # v.1.04A
# # make an mp3 from the mp4 video
# # system(paste0("ffmpeg -i ", "data/raw/groundtruth/ISS_v1.04A.mp4", 
# #               " -ar 44100 -ac 2 ", "data/raw/groundtruth/ISS_v1.04A.mp3"))
# # extract the beep window
# aud.10.4 <- readMP3("data/raw/groundtruth/ISS_v1.04A.mp3")
# plot(extractWave(aud.10.4, from = 6549600, to = 6554500), 
#      main='Task Audio Waveform')
# # Zoom in on a beep region to manually identify its duration
# # Extract a short window that likely contains the beep
# beep_window <- extractWave(aud.10.4, 
#                            from = 6549600, to = 6554500) # Adjust 'from' and 'to' as needed
# plot(beep_window@left, type='l', main='Beep Signal')
# 
# # Estimate the beep frequency using FFT (Fast Fourier Transform)
# library(seewave)
# beep_freq <- seewave::spec(beep_window, 
#                            f=aud.10.4@samp.rate, 
#                            plot=FALSE)
# beep_peak_frequency <- which.max(beep_freq)  # Get peak frequency
# # Estimate the duration of the beep by detecting its length in the waveform
# beep_env <- env(beep_window)
# beep_start <- which(beep_env > 0.5 * max(beep_env))[1]  # Start point (adjust threshold)
# beep_end <- which(beep_env > 0.5 * max(beep_env))[length(which(beep_env > 0.5 * max(beep_env)))]  # End point
# beep_duration_samples <- beep_end - beep_start  # Duration in samples
# beep_duration <- beep_duration_samples / aud.10.4@samp.rate  # Duration in seconds
# 
# v1.04.bp.freq <- beep_peak_frequency
# v1.04.bp.dur <- beep_duration
# cat("Estimated beep frequency (v.1.04):", v1.04.bp.freq, "Hz\n")
# cat("Estimated beep duration (v.1.04):", v1.04.bp.dur, "seconds\n")
# 
# v.1.10B
# make an mp3 from the mp4 video
# system(paste0("ffmpeg -i ", "data/raw/groundtruth/ISS_v_1.10_B_v3.mp4",
#               " -ar 44100 -ac 2 ", "data/raw/groundtruth/ISS_v_1.10_B_v3.mp3"))

# extract the beep window
# aud.10.10 <- readMP3("data/raw/groundtruth/ISS_v_1.10_B_v3.mp3")
# plot(extractWave(aud.10.10, from = 4973000, to = 4976700),
#      main='Task Audio Waveform')
# beep_window <- extractWave(aud.10.10,
#                            from = 4973000, to = 4976700)
# plot(beep_window@left, type='l', main='Beep Signal')
# beep_freq <- seewave::spec(beep_window,
#                            f=aud.10.10@samp.rate,
#                            plot=FALSE)
# beep_peak_frequency <- which.max(beep_freq)  # Get peak frequency
# beep_env <- env(beep_window)
# beep_start <- which(beep_env > 0.5 * max(beep_env))[1]  # Start point (adjust threshold)
# beep_end <- which(beep_env > 0.5 * max(beep_env))[length(which(beep_env > 0.5 * max(beep_env)))]  # End point
# beep_duration_samples <- beep_end - beep_start  # Duration in samples
# beep_duration <- beep_duration_samples / aud.10.10@samp.rate  # Duration in seconds
# 
# v1.10.bp.freq <- beep_peak_frequency
# v1.10.bp.dur <- beep_duration
# cat("Estimated beep frequency (v.1.10):", v1.10.bp.freq, "Hz\n")
# cat("Estimated beep duration (v.1.10):", v1.10.bp.dur, "seconds\n")
# 
# 
# rm(list = c("beep_duration",
#             "beep_duration_samples",
#             "beep_end", "beep_start", "beep_env", "beep_peak_frequency",
#             "beep_freq", "beep_window", "aud.10.10", "aud.10.4"))
# gc()



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
#  read the ISS task metadata
iss.metadata <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx", 
                                  sheet = "ISS-metadata") %>%
  mutate(start_in_sec = start_in_sec-29,
         end_in_sec = end_in_sec-29)

# keep participants of interest
participants.metadata <- readxl::read_xlsx("../shared_data/data/RPOE_meta.xlsx", 
                                           sheet = "ISS_det")
p.of.int <- participants.metadata %>% 
  filter(task_version == "1.04") %>%
  mutate(start_in_sec = start_min*60 + start_sec,
         end_in_sec = end_min*60 + end_sec) %>%
  select(te_id, devGenes_id, start_in_sec, end_in_sec)

p.1.10B <- participants.metadata %>% 
  filter(task_version == "1.10B") %>%
  mutate(start_in_sec = start_min*60 + start_sec,
         end_in_sec = end_min*60 + end_sec) %>%
  select(te_id, devGenes_id, start_in_sec, end_in_sec)
################################################################################
################################################################################
# copy iss file to my dir, make the mp3 file, and crop per task
# data.dir <- "/Dedicated/jmichaelson-sdata/MRI/RPOE"
# my.data.dir <- paste0(project.dir, "/data/raw/ISS")
# system(paste0("mkdir -p ", my.data.dir))
# 
# # v 1.04
# for (i in 1:nrow(p.of.int)) {
#   # i = 1
#   pid <- p.of.int$te_id[i]
#   raw.a.file <- gsub(" ", fixed = T, "\\ ", 
#                      list.files(paste0(data.dir, "/", pid, 
#                                        "/phenotype/speech_sample"),
#                                 pattern = ".mp3", full.names = T))
#   a.file <- paste0(my.data.dir, "/", pid, ".mp3")
#   if (!file.exists(a.file) && length(raw.a.file) == 0) { # make mp3 file if the raw is only video
#     v.file <- gsub(" ", fixed = T, "\\ ", 
#                    list.files(paste0(data.dir, "/", pid, "/phenotype/speech_sample"),
#                               full.names = T, pattern = ".mp4")[1])
#     system(paste0("ffmpeg -i ", v.file, 
#                   " -ar 44100 -ac 2 ", 
#                   my.data.dir, "/", pid, ".mp3"))
#     a.file <- paste0(my.data.dir, "/", pid, ".mp3")
#   }else if(!file.exists(a.file)) { # copy the mp3 file, if it doesn't exist in my dir
#     system(str_c("cp ", raw.a.file," ", a.file))
#   }
#   
#   # read the full mp3, and crop the task only
#   aud <- readMP3(a.file)
#   
#   
#   # method 1
#   aud.cropped <- extractWave(aud, xunit = "time",
#                              from = p.of.int$start_in_sec[i],
#                              to = p.of.int$end_in_sec[i])
#   # save full task response
#   system(paste0("mkdir -p ", project.dir,
#                 "/data/derivatives/ISS_participants-response/",
#                 pid))
#   writeWave(Wave(left = as.numeric(aud.cropped@left),
#                  samp.rate = 44100,
#                  bit = 16,
#                  pcm=T),
#             filename = paste0(project.dir,
#                               "/data/derivatives/ISS_participants-response/",
#                               pid, "/", "full-ISS-cropped_",pid, ".wav"),
#             extensible = T)
#   # get responses cropped
#   # for (j in 1:nrow(iss.metadata)) {
#   #   # j = 1
#   #   task <- paste0(pid,
#   #                  "_task-", iss.metadata$task_num[j], 
#   #                  "_", iss.metadata$word[j], "_", j)
#   #   t.aud <- extractWave(aud.cropped, 
#   #                        from = iss.metadata$start_in_sec[j], 
#   #                        to = iss.metadata$end_in_sec[j], 
#   #                        xunit = "time")
#   #   # plot(t.aud)
#   #   # play(t.aud, "play")
#   #   # saving the task output
#   #   writeWave(Wave(left = as.numeric(t.aud@left), 
#   #                  samp.rate = 44100, 
#   #                  bit = 16, 
#   #                  pcm=T), 
#   #             filename = paste0(project.dir,
#   #                               "/data/derivatives/ISS_participants-response/",
#   #                               pid, "/", task, ".wav"), 
#   #             extensible = T)
#   # }
#   
# }
# 
# 
# # V.1.10B
# for (i in 9:nrow(p.1.10B)) {
#   # i = 1
#   pid <- p.1.10B$te_id[i]
#   raw.a.file <- gsub(" ", fixed = T, "\\ ", 
#                      list.files(paste0(data.dir, "/", pid, 
#                                        "/phenotype/speech_sample"),
#                                 pattern = ".mp3", full.names = T))
#   a.file <- paste0(my.data.dir, "/", pid, ".mp3")
#   if (!file.exists(a.file) && length(raw.a.file) == 0) { # make mp3 file if the raw is only video
#     v.file <- gsub(" ", fixed = T, "\\ ", 
#                    list.files(paste0(data.dir, "/", pid, "/phenotype/speech_sample"),
#                               full.names = T, pattern = ".mp4")[1])
#     system(paste0("ffmpeg -i ", v.file, 
#                   " -ar 44100 -ac 2 ", 
#                   my.data.dir, "/", pid, ".mp3"))
#     a.file <- paste0(my.data.dir, "/", pid, ".mp3")
#   }else if(!file.exists(a.file)) { # copy the mp3 file, if it doesn't exist in my dir
#     system(str_c("cp ", raw.a.file," ", a.file))
#   }
#   
#   # read the full mp3, and crop the task only
#   aud <- readMP3(a.file)
#   
#   
#   # method 1
#   aud.cropped <- extractWave(aud, xunit = "time",
#                              from = p.1.10B$start_in_sec[i],
#                              to = (p.1.10B$end_in_sec[i] + 5))
#   # save full task response
#   system(paste0("mkdir -p ", project.dir,
#                 "/data/derivatives/ISS_participants-response/",
#                 pid))
#   writeWave(Wave(left = as.numeric(aud.cropped@left),
#                  samp.rate = 44100,
#                  bit = 16,
#                  pcm=T),
#             filename = paste0(project.dir,
#                               "/data/derivatives/ISS_participants-response/",
#                               pid, "/", "full-ISS-cropped_",pid, ".wav"),
#             extensible = T)
#   # get responses cropped
#   # for (j in 1:nrow(iss.metadata)) {
#   #   # j = 1
#   #   task <- paste0(pid,
#   #                  "_task-", iss.metadata$task_num[j], 
#   #                  "_", iss.metadata$word[j], "_", j)
#   #   t.aud <- extractWave(aud.cropped, 
#   #                        from = iss.metadata$start_in_sec[j], 
#   #                        to = iss.metadata$end_in_sec[j], 
#   #                        xunit = "time")
#   #   # plot(t.aud)
#   #   # play(t.aud, "play")
#   #   # saving the task output
#   #   writeWave(Wave(left = as.numeric(t.aud@left), 
#   #                  samp.rate = 44100, 
#   #                  bit = 16, 
#   #                  pcm=T), 
#   #             filename = paste0(project.dir,
#   #                               "/data/derivatives/ISS_participants-response/",
#   #                               pid, "/", task, ".wav"), 
#   #             extensible = T)
#   # }
#   
#   
#   
#   # Method 2
#   # it works, but not optimal
#   # use beep freq info to extract first beep
#   sampling_rate <- aud@samp.rate
#   beep_sample_length <- v1.10.bp.dur * sampling_rate
#   # Analyze the participant's audio for the extracted beep frequency
#   freq_analysis <- ffilter(aud,
#                            from = v1.10.bp.freq - 50,
#                            to = v1.10.bp.freq + 50,
#                            bandpass=T)
#   # Extract envelope to identify potential beep signals
#   env_signal <- env(freq_analysis, f = aud@samp.rate)
#   # Find peaks that match the beep's characteristics
#   threshold <- 0.9  # Adjust based on audio characteristics
#   beep_times <- which(env_signal > threshold)
#   # Convert sample indices to time in seconds
#   beep_time_seconds <- beep_times / sampling_rate
#   # Print or store detected beep times
#   print(beep_time_seconds)
# }


################################################################################
################################################################################
################################################################################
# library(seewave)
# library(fftw)
# library(tuneR)
# # Load task audio (contains the beep)
# task_audio <- readMP3("data/raw/groundtruth/ISS_v_1.10_B_v3.mp3")
# task_mono <- mono(task_audio, which = "both")
# 
# plot(task_mono)
# # plot(extractWave(task_audio, from = 4973000, to = 4976700),
# #      main='Task Audio Waveform')
# # beep_sound <- extractWave(task_audio, from = 4973000, to = 4976700)
# # writeWave(beep_sound, "data/raw/ISS_beep-sound.wav")
# # beep_mono <- mono(beep_sound)
# 
# 
# # Load participant's recorded audio
# participant_audio <- readMP3(a.file)
# participant_audio_mono <- mono(participant_audio, which = "both")
# 
# # extract wave samples
# task_samples <- task_mono@left
# participant_samples <- participant_audio_mono@left
# 
# length(participant_samples) - length(task_samples)
# registerDoMC(cores = 4)
# beep.mapping <- foreach(ii = 1:(length(participant_samples) - length(task_samples)), .combine = rbind) %dopar% {
#   p.mono.cr <- participant_samples[ii:(ii+(length(task_samples)-1))]
#   t.df <- data.frame(task = task_samples, participant = p.mono.cr) %>%
#     mutate(participant_2 = ifelse(task == 0, 0, participant))
#   data.frame(index = ii,
#              cor_with_p = cor(t.df$task, t.df$participant),
#              cor_with_p2 = cor(t.df$task, t.df$participant_2))
# }




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
source("src/ISS/JM/code_for_muhammad.R")
pload("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")


# copy iss file to my dir, make the mp3 file, and crop per task
data.dir <- "/Dedicated/jmichaelson-sdata/MRI/RPOE"
my.data.dir <- paste0(project.dir, "/data/raw/ISS")
system(paste0("mkdir -p ", my.data.dir))

registerDoMC(cores = 6)
foreach(ii = 1:nrow(p.1.10B)) %dopar% {
  pid <- p.1.10B$te_id[ii]
  raw.a.file <- gsub(" ", fixed = T, "\\ ", list.files(paste0(data.dir, "/", pid,  "/phenotype/speech_sample"),pattern = ".mp3", full.names = T))
  a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  if (!file.exists(a.file) && length(raw.a.file) == 0) { # make mp3 file if the raw is only video
    v.file <- gsub(" ", fixed = T, "\\ ", list.files(paste0(data.dir, "/", pid, "/phenotype/speech_sample"),full.names = T, pattern = ".mp4")[1])
    system(paste0("ffmpeg -i ", v.file, " -ar 44100 -ac 2 ", my.data.dir, "/", pid, ".mp3"))
    a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  }else if(!file.exists(a.file)) { # copy the mp3 file, if it doesn't exist in my dir
    system(str_c("cp ", raw.a.file," ", a.file))
  }
  # read the audio file and align to template
  p.mp3 <- readMP3(a.file)
  
  p.aligned <- align_to_template(p.mp3,idx=idx)
  # p.1.10B$start_in_sec[ii]
  # plot(p.aligned[ann[[1]][4]:ann[[2]][4]])
  # plot(p.aligned[ann[[1]][50]:ann[[2]][99]])
  # abline(v=ann$seconds[50:99]-424.5,col='red')
  
  ## save the cropped audio
  system(paste0("rm -rf ", project.dir, "/data/derivatives/ISS_participants-response/",pid))
  system(paste0("mkdir -p ", project.dir, "/data/derivatives/ISS_participants-response/",pid))
  writeWave(p.aligned,
            filename = paste0(project.dir,"/data/derivatives/ISS_participants-response/",pid, "/", pid, "_ISS-FULL.wav"),
            extensible = T)
  writeWave(extractWave(p.aligned, from = 0, to = 150*44100),
            filename = paste0(project.dir,"/data/derivatives/ISS_participants-response/",pid, "/", pid, "_ISS-pt1.wav"),
            extensible = T)
  writeWave(extractWave(p.aligned, from = (150*44100)+1, to = 300*44100),
            filename = paste0(project.dir,"/data/derivatives/ISS_participants-response/",pid, "/", pid, "_ISS-pt2.wav"),
            extensible = T)
  writeWave(extractWave(p.aligned, from = (300*44100)+1, to = length(p.aligned)),
            filename = paste0(project.dir,"/data/derivatives/ISS_participants-response/",pid, "/", pid, "_ISS-pt3.wav"),
            extensible = T)
}



# extract the features
jm.feat <- foreach(ii=1:nrow(p.1.10B), .combine = rbind) %dopar% {
  pid <- p.1.10B$te_id[ii]
  p.aligned <- readWave(paste0(project.dir,"/data/derivatives/ISS_participants-response/",pid, "/", pid, "_ISS-FULL.wav"))
  
  # extract the time for reading words, responding to checkbox and hi prompts
  if (ii%in%c(2,36)) {x=2000} else {x = 3000}
  cb.stat <- sapply(c(x),function(y) vo_rxn(p.aligned, ann, thresh = y, plot.it = F))
  hi.stat <- sapply(c(200),function(y) tryCatch({ao_rxn(p.aligned, ann, thresh = y, plot.it = F)}))
  wr.stat <- reading_time(p.aligned, ann, plot.it = F)
  res.df <- wr.stat %>% as.data.frame() %>%
    rownames_to_column("word") %>%
    rename(value = 2) %>%
    mutate(task = "reading") %>% relocate(task) %>%
    rbind(data.frame(task = "checkbox",
                     word = c("median", "mad"),
                     value = c(as.numeric(cb.stat[1]), as.numeric(cb.stat[2])))) %>%
    rbind(data.frame(task = "hi",
                     word = c("median", "mad"),
                     value = c(as.numeric(hi.stat[1]), as.numeric(hi.stat[2])))) %>%
    mutate(te_id = pid)
  return(res.df)
}
write_csv(jm.feat, "data/derivatives/ISS-v110B-word-reading-timing.csv")

tt <- jm.feat %>%
  filter(word == "median") %>%
  pivot_wider(names_from = task, values_from = value, id_cols = te_id) 
View(tt)
plot(tt$hi,tt$checkbox,ylim=c(0,600),xlim=c(0,600),ylab="visual-vocal RT (median ms)",
     xlab="auditory-vocal RT (median ms)")
abline(0,1,col='grey')
abline(lm(tt$checkbox~tt$hi),col='orangered',lty=2)



################################################################################

################################################################################