extract_ISS_110_responses_timing <- function(audio_file,
                                             word_detection_frame=0.005,
                                             eneregy_envelope_rate=0.1,
                                             check=T,hi=T,word_reading=F,
                                             offset_only=F) {
  
  ################################################################################
  require(tuneR)
  require(seewave)
  require(signal)
  pload("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language/src/ISS/JM/template_and_ann_for_1.10_v3.Rdata.pxz")
  ################################################################################
  
  
  # Read the audio file
  audio <- readMP3(audio_file)
  
  # Extract audio data
  audio_signal <- audio@left
  sample_rate <- audio@samp.rate
  
  # Define known events from your task (in seconds from task start)
  task_events <- data.frame(
    event = paste(ann$task, ann$prompt, ann$type, sep="___"),
    task_time = c(ann$seconds)  # your actual timings
  )
  
  # Method 1: Cross-correlation with template sounds
  align_audio <- function(audio_signal, sample_rate, template_file, task_time) {
    # Read template (instruction audio from your task)
    template <- readMP3(template_file)
    template_signal <- template@left
    
    # Cross-correlation to find template in participant audio
    correlation <- ccf(audio_signal, template_signal, plot = T)
    max_corr_idx <- which.max(correlation$acf)
    offset <- correlation$lag[max_corr_idx] / sample_rate
    
    return(offset)
  }
  
  # Method 2: Energy-based detection of instructions
  find_instruction_onset <- function(audio_signal, sample_rate) {
    # Calculate energy envelope
    frame_size <- eneregy_envelope_rate * sample_rate  # 100ms frames
    energy <- sapply(seq(1, length(audio_signal) - frame_size, by = frame_size), 
                     function(i) sum(audio_signal[i:(i+frame_size)]^2))
    
    # Find significant energy increase (instruction start)
    energy_threshold <- mean(energy) + 5 * sd(energy)
    instruction_start <- which(energy > energy_threshold)[1] * (frame_size/sample_rate)
    
    return(instruction_start)
  }
  
  # Calculate offset
  instruction_offset <- find_instruction_onset(audio_signal, sample_rate)
  # Or use cross-correlation if you have clean template
  # instruction_offset <- align_audio(audio_signal, sample_rate, "data/raw/groundtruth/ISS_v_1.10_B_v3.mp3", 0)
  if (offset_only) {
    return(instruction_offset)
  }
  
  # Align all events
  task_events$audio_time <- task_events$task_time + instruction_offset
  
  # Detect responses
  detect_responses <- function(audio_signal, sample_rate, event_times, response_type = "check") {
    responses <- data.frame()
    
    for (i in 1:length(event_times)) {
      event_time <- event_times[i]
      
      # Extract audio segment after event (e.g., 0-3 seconds after)
      start_sample <- round(event_time * sample_rate)
      end_sample <- round((event_time + 5) * sample_rate)
      segment <- audio_signal[start_sample:min(end_sample, length(audio_signal))]
      
      if (response_type == "check") {
        # Detect "check" responses (look for specific phonetic patterns)
        response_detected <- detect_word(segment, sample_rate, "check")
      } else {
        # Detect "hi" responses
        response_detected <- detect_word(segment, sample_rate, response_type)
      }
      
      if (!is.na(response_detected)) {
        response_time <- event_time + response_detected
        responses <- rbind(responses, data.frame(
          event_index = i,
          stimulus_time = event_time,
          response_time = response_time,
          reaction_time = response_detected
        ))
      }
    }
    
    return(responses)
  }
  
  # Simple word detection using energy peaks
  detect_word <- function(segment, sample_rate, word) {
    # Calculate short-term energy
    frame_length <- word_detection_frame * sample_rate  # 25ms frames
    energy <- sapply(seq(1, length(segment) - frame_length, by = frame_length/2),
                     function(i) mean(abs(segment[i:min(i+frame_length, length(segment))])))
    
    # Find energy peaks (simplified approach)
    peaks <- which(diff(sign(diff(energy))) < 0) + 1
    if (length(peaks) > 0) {
      first_peak <- peaks[1] * (frame_length/2) / sample_rate
      return(first_peak)
    }
    return(NA)
  }
  
  ################################################################################
  ################################################################################
  # Detect all responses
  if (check) {
    checkbox_responses <- detect_responses(audio_signal, sample_rate, 
                                           task_events$audio_time[task_events$event %like% "CHECKBOX___check"], 
                                           "check") %>%mutate(task = "check")
  }else{checkbox_responses=NULL}
  
  if (hi) {
    hi_responses <- detect_responses(audio_signal, sample_rate,
                                     task_events$audio_time[task_events$event %like% "HI___hi"], 
                                     "hi") %>%mutate(task = "hi")
  }else{hi_responses=NULL}
  
  if (word_reading) {
    word.reading.responses <- do.call(rbind,lapply(ann$prompt[50:99], function(pp){
      detect_responses(audio_signal, sample_rate,
                       task_events$audio_time[task_events$event %like% pp],
                       pp)%>%mutate(prompt=pp)})) %>%mutate(task = "word_reading")
  } else {word.reading.responses=NULL}
  
  return(rbind(checkbox_responses, hi_responses,word.reading.responses))

}
