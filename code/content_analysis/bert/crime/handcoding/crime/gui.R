# ______________________________________________
# Media rections to RR violence
# Goal: Write GUI for sample coding
# Procedure: draw sample, annotate, save
# ______________________________________________
# Date:  Wed Apr 28 17:26:26 2021
# Author: Nicolai Berk
#  R version 4.0.3 (2020-10-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(here)
library(data.table)
library(shiny)


# setup ui

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      
      
    body {
      background-color:white;
      font-family: 'Lato', sans-serif;
    }
    
    body{
    color: black; 
    text-align:justify;
    font-size:16px;
    }
    
    img {
      vertical-align: bottom;
    }
    
    ul {
      list-style-type: none;
      margin: 0;
      padding: 0;
    }
    
    h1, .h1 {
      display: none;
    }
    
    
    h2, .h2 {
      color: #0e5484;
      text-decoration: underline;
    }
    
    h3, .h3  {
        margin-top: 18px;
        color: #0e5484;
    }
    
    h4, .h4  {
        margin-top: 12px;
          color: #0e5484;
    }
    
    /* link */
    a, .navbar-default .navbar-nav > .active > a {
      color: #0e5484;
    }
    
    a:active, a:hover{
      color: #0e5484;
      text-decoration: underline;
    }
    
    a p {
      display:none;
    }
    
    a:hover p {
      display:block;
    }
    
    span {
      color: #0e5484;
      font-weight: bold;
    }
    
    .btn {
        border-width: 0 0 0 0;
        font-weight: bold;
    }
    
    /* brand name in menu */
    .navbar-default .navbar-brand {
      color: #0e5484;
    }
    
    .btn-default, .nav-pills:active>a {
        color: white;
        background-color: #6892D3;
        border-color: white;
    }
    
    .navbar-default{
      background-color: #6892D3;
    }
    
    .nav-pills:hover>a, .nav-pills>li>a, .navbar-default .navbar-nav>li>a:hover  {
        color: white;
        background-color: #0e5484;
        border-color: white;
    }"))
  ),
  
  # Application title
  titlePanel(paste0("Handkodierung Nachrichtenartikel")),
  
  
  conditionalPanel(
    condition = "input.start != 1",
    actionButton("start", "Start")),
  
  # Text output and questions
  conditionalPanel(
    condition = "input.start == 1", 
    fluidPage(
      
      column(
        7,
      
      br(),
      'Bitte lies den Titel unten (falls notwendig auch den Rest des Texts) und beantworte die Fragen. Wenn Du fertig bist, klicke bitte auf "Weiter".',
      br(), br(),
      h3(textOutput("progress")),
      br(),
      
      h3(textOutput("Title")),
      
      fluidPage(textOutput("Text")),
      
      br(),
      br()),
      
      column(
        5,
        
        h3("Nimmt der Artikel Bezug auf das Thema Kriminalität, Law and Order, oder Öffentliche Sicherheit?"),
        
        br(),
        
        radioButtons("crime", 
                     label = "",
                     selected = "Bitte wählen",
                     choices = c("Bitte wählen", "Ja", "Nein")),
        
        # conditionalPanel(
        #   condition = 'input.crime == "Unklar"',
        #   textInput("crime_unklar", 
        #             label = "Weshalb unklar/über welches Thema spricht der Artikel?")
        # ),
        
        
        ### extended definition
        checkboxInput("crime_def", "Zeige erweiterte Definition.", value = F),
        conditionalPanel(
          condition = 'input.crime_def == 1',
          fluidPage(p("Hierzu zählen Berichte über (mutmaßliche) Straftaten, Polizeiberichte, Statistiken über Straftaten, potenzielle Straftäter:innen, Artikel über die Sicherheitslage und/oder Gerichtsprozesse."))
        ),
          
        div(),
      
        conditionalPanel(
          condition = "input.crime != 'Bitte wählen' && input.rr != 'Bitte wählen' && input.migration != 'Bitte wählen'",
          actionButton("nextpage", "Weiter", icon = icon("check"))
        ),
        
        br(),
        
        
        conditionalPanel(
            condition = "input.nextpage > 0",
            actionButton("back", "Zurück" , icon = icon("reply"))
        ),
        
        br(),
        
        # add "save button"
        actionButton("save", "Daten speichern.", icon = icon("download"))
             
             
        # h3("Ist das Hauptthema des Artikels Migration oder Migrationspolitik?"),
        # 
        # br(),
        # 
        # radioButtons("migration", 
        #              label = "",
        #              selected = "Bitte wählen",
        #              choices = c("Bitte wählen", "Ja", "Nein", "Unklar")),
        # 
        # conditionalPanel(
        #   condition = 'input.migration == "Unklar"',
        #   textInput("migration_unklar", 
        #             label = "Weshalb unklar/über welches Thema spricht der Artikel?")
        # ),
        # 
        # ### extended definition
        # checkboxInput("mig_def", "Zeige erweiterte Definition.", value = F),
        # conditionalPanel(
        #   condition = 'input.mig_def == 1',
        #   fluidPage(p("Hierzu zählen bspw. Flüchtlingszahlen, Diskussionen um Zuwanderungs- und Einbürgerungsrechte, allgemeine Berichte über globale Migration, Reformen in der Migrationspolitik..."))
        # ),
        # 
        # br(),
        # br(),
        
            
        
        # br(),
        # br(),
        # 
        # h3("Nimmt der Artikel Bezug auf das Thema Rechtsradikalismus?"),
        # 
        # br(),
        # 
        # radioButtons("rr", 
        #              label = "",
        #              selected = "Bitte wählen",
        #              choices = c("Bitte wählen", "Ja", "Nein", "Unklar")),
        # 
        # conditionalPanel(
        #   condition = 'input.rr == "Unklar"',
        #   textInput("rr_unklar", 
        #             label = "Weshalb unklar/über welches Thema spricht der Artikel?")
        # ),
        #       
        #     
        # ### extended definition
        # checkboxInput("rr_def", "Zeige erweiterte Definition.", value = F),
        # conditionalPanel(
        #   condition = 'input.rr_def == 1',
        #   fluidPage(p("Hierzu zählen sowohl Artikel über rechtsradikale Parteien, wie beispielsweise die AfD, aber auch über rechtsradikalen Terror, xenophobe Angriffe/Anschläge, migrationskritische/-feindliche Demonstrationen oder Gruppierungen. Die Ledigliche Nennung ist nicht ausreichend."))
        # ),
        # 
    

))))


server <- function(input, output, session) {
  
  observeEvent(input$start, {
    
    showModal(modalDialog(
      title = "Wie heisst Du?  (schreib den Namen jedes Mal gleich!)",
      textInput("name", "Name", placeholder = "Name"),
      footer = tagList(actionButton("newstart", "Starte neu."),
                       actionButton("loadfile", "Lade existierendes file..."))
    ))
  })
  
  observeEvent(input$loadfile, {
    
    
    # import sample
    tryCatch(
      {
        dta <<- fread(paste0("handcoding_", input$name, ".csv"), encoding = "UTF-8", na.strings = NULL)
        dta$text <<- sapply(dta[,"text"], gsub, pattern = '\"', replacement = "") # fixes issue with duplication of quotation marks
        dta$title <<- sapply(dta[,"title"], gsub, pattern = '\"', replacement = "") # fixes issue with duplication of quotation marks
        
      }, error = function(e){
        showModal(modalDialog(
          title = paste0("Keine existierende Datei gefunden: 'handcoding_", input$name, ".csv'. Sicher, dass Du den Namen korrekt angegeben hast?"),
          textInput("name", "Name", placeholder = "Name"),
          footer = tagList(actionButton("newstart", "Starte neu."),
                           actionButton("loadfile", "Lade existierendes file..."))
        ))
      })
    
    # dta$crime_unklar[is.na(dta$crime_unklar)] <<- "" # ensures correct data format when importing
    
    # define initial text
    counter <<- (1 + nrow(dta) - sum(dta$crime == "")) # starting point  = 1 + number of rows - number of uncoded rows 
    rowsToCode <<- sum(dta$crime == "")
    
    output$Title <- renderText(as.character(dta$title[counter]))
    output$Text <- renderText(as.character(dta$text[counter]))
    
    removeModal()
    
  })
  
  
  observeEvent(input$newstart, {
    
    # import sample
    dta <<- fread("crime_sample.csv", encoding = "UTF-8")[, c("title", "url", "text")]
    dta <<- sample_n(dta, nrow(dta))  # reorder rows
    dta$text <<- sapply(dta[,"text"], gsub, pattern = '""', replacement = '"') # fixes issue with duplication of quotation marks
    dta$title <<- sapply(dta[,"title"], gsub, pattern = '""', replacement = '"') # fixes issue with duplication of quotation marks
    
    # define initial text
    counter <<- 1
    rowsToCode <<- nrow(dta)
    
    output$Title <- renderText(as.character(dta$title[counter]))
    output$Text <- renderText(as.character(dta$text[counter]))
    
    dta$crime           <<- NA_character_
    # dta$crime_unklar    <<- NA_character_
    # dta$mig             <<- NA_character_
    # dta$rr              <<- NA_character_
    
    removeModal()
  })
  
  output$progress <- renderText({
    paste("Coding #", (input$nextpage+1-input$back), "of", ifelse(exists("rowsToCode"), rowsToCode, ""), "remaining articles.")
  })
  
  
  observeEvent(input$save, {
    
    showModal(modalDialog(
      title="Sicher, dass Du speichern und beenden willst?",
      tagList(actionButton("save_fin", "Speichern", icon = icon("check")),
              modalButton("Cancel", icon = icon("remove"))
      )
    ))
  })
  
  observeEvent(input$save_fin, {
    
    fwrite(dta, file = paste0("handcoding_", input$name, ".csv"))
    
    showModal(modalDialog(
      title="Daten gespeichert, Du kannst den Browser jetzt schliessen"
    ))
    
    Sys.sleep(5)
    
    stopApp()
    
  })
  
  observeEvent(input$nextpage, {
    
    
    # hide text
    updateCheckboxInput(session, inputId = "more", value = F)
    
    # write changes
    dta[counter, "crime"]           <<- input$crime
    # dta[counter, "crime_unklar"]    <<- input$crime_unklar
    # dta[counter, "migration"]       <<- input$migration
    # dta[counter, "migration_unklar"]<<- input$migration_unklar
    # dta[counter, "rr"]              <<- input$rr
    # dta[counter, "rr_unklar"]       <<- input$rr_unklar
    
    if (counter < nrow(dta)){
      
      # call next row
      counter <<- counter + 1
      output$Title <- renderText(as.character(dta$title[counter]))
      output$Text <- renderText(as.character(dta$text[counter]))
      
      # Update radiobuttons and textfields
      for (but in c("crime")){
        updateRadioButtons(session = session, inputId =  but, selected = "Bitte wählen")
        updateTextInput(session = session, inputId = paste0(but, "_unklar"), value = "")
      }
      
      
    }else{
      
      showModal(modalDialog(
        title="Kodierung abgeschlossen!",
        tagList(
          actionButton("save_fin", "Speichern und schliessen.", icon = icon("download"))
        )
      ))
      
    }
    
    
  })
  
  observeEvent(input$back, {
    counter <<- counter - 1
    output$Title <- renderText(as.character(dta$title[counter]))
    output$Text <- renderText(as.character(dta$text[counter]))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

