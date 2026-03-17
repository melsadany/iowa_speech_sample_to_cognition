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
## clean the transcription data
raw.tx <- readxl::read_xlsx("data/RPOE_meta.xlsx", sheet = "PSVC_tx_whisperX") %>%
  mutate(response = tolower(response), prompt = tolower(prompt), text_revised = tolower(text_revised),
         number_response = ifelse(grepl("[0-9]",response), 
                                  as.character(english::english(as.numeric(str_replace_all(response,"th|nd|rd|-","")))), NA)) %>%
  filter(!te_id %in% c("2E_040", "2E_022")) # drop

################################################################################
################################################################################
## extract uncertainity / um/uh/hmm couts
ums <- raw.tx %>% 
  filter(prompt!=response, # to drop the "a" that is in the prompt A
         response %in% c("uh", "um", "umm", "oh", "eh", "hmm", "hmmm", "a", "mmm")) %>%
  group_by(te_id, prompt) %>% summarise(ums_count = n())
write_rds(ums, "data/derivatives/tmp-use/PSVC-ums-count.rds", compress = "gz")

## count comments
comments <- raw.tx %>% 
  group_by(te_id, prompt) %>%
  summarise(comment_count = sum(comment == "T", na.rm=T))
write_rds(comments, "data/derivatives/tmp-use/PSVC-comments-count.rds", compress = "gz")

## drop ums, comments, and false tx words
clean.tx.0 <- raw.tx %>% 
  mutate(response = str_replace_all(ifelse(is.na(text_revised), response, text_revised), "\\?",""),
         response = ifelse(is.na(text_revised)&!is.na(number_response), number_response, response)) %>%
  filter(is.na(text_revised)|text_revised!="f", 
         !response %in% c("uh", "um", "umm", "oh", "eh", "hmm", "hmmm", "a", "mmm","W?", "yeah", "okay"),
         is.na(comment)) %>%
  mutate(start = ifelse(is.na(start)|start<0, start_o, start),
         end = ifelse(is.na(end), end_o, end),
         duration = end-start,
         end = ifelse(duration>8&!is.na(start_o), start+(end_o-start_o),end),
         duration = end-start) %>%
  select(-c(start_o, end_o,text_revised,comment,number_response))

## count repeated prompts
rep.prompt <- clean.tx.0 %>% 
  group_by(te_id, prompt) %>%
  summarise(rep_prompt = sum(prompt==response,na.rm=T))
write_rds(rep.prompt, "data/derivatives/tmp-use/PSVC-repeated-prompt-count.rds", compress = "gz")

## count repeated words per prompt
rep.words.0 <- clean.tx.0 %>% 
  group_by(te_id, prompt) %>%
  summarise(rep_words_per_prompt = n()-length(unique(response)))
write_rds(rep.words.0, "data/derivatives/tmp-use/PSVC-repeated-words-per-prompt-count.rds", compress = "gz")

## count repeated words in the entire task
rep.words.1 <- clean.tx.0 %>% 
  group_by(te_id) %>%
  summarise(rep_words = n()-length(unique(response)))
write_rds(rep.words.1, "data/derivatives/tmp-use/PSVC-repeated-words-in-the-entire-task-count.rds", compress = "gz")

## clean tx again by dropping repeated words per prompt and repeated prompts
clean.tx.1 <- clean.tx.0 %>%
  filter(prompt!=response) %>%
  distinct(te_id, prompt, response, .keep_all = T)
write_rds(clean.tx.1, "data/derivatives/PSVC-clean-transcription.rds", compress = "gz")
################################################################################
################################################################################