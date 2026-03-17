library(shiny)
library(tuneR)
library(audio)
library(DT)
source("/wdata/msmuhammad/msmuhammad-source.R")

options(shiny.maxRequestSize = 100 * 1024^2)  # Increase upload limit to 100MB

ui <- fluidPage(
  titlePanel("Transcription Validation"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_initials", "Enter Your Initials:", ""),
      fileInput("audio_file", "Upload Participant Audio", accept = c(".wav")),
      actionButton("start", "Start Reviewing")
    ),
    mainPanel(
      textOutput("current_word"),
      actionButton("correct", "Correct"),
      textInput("new_word", "Enter Correct Word (if incorrect)", ""),
      actionButton("next_word", "Next"),
      downloadButton("download", "Download Results")
    )
  )
)

server <- function(input, output, session) {
  
  transcription_data <- reactiveVal()
  current_index <- reactiveVal(1)
  results <- reactiveVal(data.frame())
  audio_data <- reactiveVal(NULL)
  
  observeEvent(input$audio_file, {
    req(input$audio_file)
    audio_data(readWave(input$audio_file$datapath, from = 0, to = Inf, units = "seconds"))
  })
  
  observeEvent(input$start, {
    df0 <- read_tsv("/wdata/msmuhammad/projects/RPOE/language/data/derivatives/ISS_transcription/v110B-tx-raw.tsv")
    
    df <- df0 %>%
      filter(te_id == "2E_007") %>%
      select(start = raw_start, end = raw_end, word = text)
    transcription_data(df)
    current_index(1)
  })
  
  
  
  observeEvent(input$next_word, {
    df <- transcription_data()
    if (!is.null(df) && current_index() <= nrow(df)) {
      start_time <- df$start[current_index()] / 1000  # Convert ms to seconds
      end_time <- df$end[current_index()] / 1000
      
      if (!is.null(audio_data())) {
        segment <- extractWave(audio_data(), from = start_time, to = end_time, xunit = "time")
        saveWave(segment, "temp.wav")
        system("ffplay -autoexit temp.wav", wait = FALSE)
        
      }
      
      new_entry <- data.frame(
        start = df$start[current_index()],
        end = df$end[current_index()],
        word = df$word[current_index()],
        initials = input$user_initials,
        corrected_word = ifelse(input$new_word != "", input$new_word, df$word[current_index()]),
        stringsAsFactors = FALSE
      )
      results(rbind(results(), new_entry))
      current_index(current_index() + 1)
    }
  })
  
  output$current_word <- renderText({
    df <- transcription_data()
    if (!is.null(df) && current_index() <= nrow(df)) {
      paste("Word:", df$word[current_index()])
    } else {
      "No more words to review."
    }
  })
  
  output$download <- downloadHandler(
    filename = function() { paste0("transcription_review_", input$user_initials, ".csv") },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
