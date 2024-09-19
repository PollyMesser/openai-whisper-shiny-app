library(shiny)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)

ui <- fluidPage(
  useShinyalert(),  # Für Benachrichtigungen
  useShinyjs(),     # Für shinyjs
  
  titlePanel("Audio Transkription mit Whisper"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("audio", "Wähle eine Audiodatei", accept = c(".mp3", ".wav", ".ogg")),
      selectInput("language", "Sprache auswählen", choices = c("Englisch" = "English", "Deutsch" = "German", "Französisch" = "French")),
      selectInput("format", "Ausgabeformat auswählen", choices = c("Text" = "txt", "SRT" = "srt")),
      actionButton("transcribe", "Transkription starten")
    ),
    
    mainPanel(
      withSpinner(textOutput("transcript")),  # Zeige den Ladebalken, während die Transkription läuft
      uiOutput("download_ui")  # Dynamisch erzeugter Download-Button
    )
  )
)

server <- function(input, output, session) {
  transcribed_text <- reactiveVal(NULL)
  
  observeEvent(input$transcribe, {
    req(input$audio)
    
    # Zeige einen dauerhaften Shinyalert an, solange die Transkription läuft
    shinyalert("Die Transkription läuft, bitte warten...", type = "info", showConfirmButton = FALSE, closeOnClickOutside = FALSE)
    
    # Pfad der hochgeladenen Datei
    audio_path <- normalizePath(input$audio$datapath, winslash = "/")
    
    # Führe Whisper aus (dieser Teil dauert länger)
    result <- tryCatch({
      model$transcribe(audio_path, language = input$language)
    }, error = function(e) {
      shinyalert("Fehler bei der Transkription", type = "error")
      return(NULL)
    })
    
    # Wenn die Transkription erfolgreich war
    if (!is.null(result)) {
      transcribed_text(result$text)
      
      # Schließe den vorherigen Shinyalert
      runjs("swal.close();")  # Verwende JavaScript, um den Shinyalert zu schließen
      
      # Zeige eine Erfolgsmeldung an und aktiviere den Download-Button
      shinyalert("Transkription abgeschlossen!", type = "success")
      
      # Render den Download-Button dynamisch
      output$download_ui <- renderUI({
        downloadButton("download", "Transkript herunterladen")
      })
    }
  })
  
  # Zeige das Transkript an
  output$transcript <- renderText({
    req(transcribed_text())
    transcribed_text()
  })
  
  # Ermögliche das Herunterladen des Transkripts
  output$download <- downloadHandler(
    filename = function() {
      if (input$format == "txt") {
        "transkript.txt"
      } else {
        "transkript.srt"
      }
    },
    content = function(file) {
      writeLines(transcribed_text(), file)
    }
  )
}

shinyApp(ui, server)
