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
pload("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")
################################################################################
################################################################################
################################################################################
################################################################################
samples <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "ISS_det") %>%
  dplyr::select(devGenes_id, te_id, ISS, task_version) %>%
  dplyr::filter(ISS=="T", task_version=="1.10B") %>%
  rowwise() %>%
  mutate(file_path = list.files(full.names = T, paste0("/Dedicated/jmichaelson-sdata/MRI/RPOE/", te_id, "/phenotype/speech_sample/"),pattern = "mp3")[1],
         ex = file.exists(file_path)) %>% dplyr::filter(ex==T) %>% select(-ex) %>%
  mutate(processed = file.exists(paste0("data/derivatives/ISS_transcription/110B/", te_id, ".tsv")))
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
## get offset
source("src/utils/ISS_110B_alignment.R")
registerDoMC(30)
iss.offset <- foreach(ss=1:nrow(samples),.combine = rbind) %dopar% {
  id.n <- samples$te_id[ss]
  data.frame(te_id = id.n, offset=extract_ISS_110_responses_timing(audio_file = samples$file_path[ss],offset_only = T))
}
iss.offset %>% write_rds("data/derivatives/tmp-use/ISS/audio-offset.rds", compress = "gz")
################################################################################
################################################################################
source("src/utils/ISS_110B_alignment.R")
registerDoMC(8)
iss.timings <- foreach(ss=1:nrow(samples),.combine = rbind) %dopar% {
  id.n <- samples$te_id[ss]
  extract_ISS_110_responses_timing(audio_file = samples$file_path[ss]) %>%
    mutate(te_id = id.n)
}
view(iss.timings %>% group_by(te_id, task) %>% summarise(c=n()))
## the results have some extra points from the task
## the first check and the last 2 "hi" are not the participants response
iss.timings.clean <- rbind(iss.timings%>%dplyr::filter(task=="check")%>%group_by(te_id)%>%slice_tail(n=5),
                           iss.timings%>%dplyr::filter(task=="hi")%>%group_by(te_id)%>%slice_head(n=5))
iss.timings.clean %>% write_rds("data/derivatives/tmp-use/ISS/checkbox-hi-reaction-time.rds",compress = "gz")

iss.timings.clean %>%ggplot(aes(reaction_time)) + geom_histogram()+ facet_wrap(~task)+bw.theme
iss.timings.clean %>%ggplot(aes(reaction_time)) + geom_histogram()+ ggh4x::facet_grid2(rows=vars(te_id),cols=vars(task))+bw.theme
################################################################################
################################################################################
## extract word reading gap
source("src/utils/ISS_110B_alignment.R")
registerDoMC(30)
word.reading.timings <- foreach(ss=1:nrow(samples),.combine = rbind) %dopar% {
  id.n <- samples$te_id[ss]
  extract_ISS_110_responses_timing(audio_file = samples$file_path[ss],
                                   eneregy_envelope_rate=0.0005,word_detection_frame = 0.0005,
                                   check = F,hi = F,word_reading = T) %>%
    mutate(te_id = id.n)%>%dplyr::filter(response_time>400) #help has two points detected, one of them is incorrect
}
view(word.reading.timings%>%group_by(te_id)%>%summarise(c=n()))
word.reading.timings%>%left_join(ann%>%dplyr::filter(task=="READING")%>%select(prompt,type)%>%mutate_all(.funs = function(x)tolower(x))) %>%
  write_rds("data/derivatives/tmp-use/ISS/word-reading-response-times.rds",compress = "gz")
################################################################################
################################################################################
