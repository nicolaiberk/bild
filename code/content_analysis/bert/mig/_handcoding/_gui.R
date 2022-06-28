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
      
      br(),
      'Bitte lies den Titel unten (falls notwendig auch den Rest des Texts) und beantworte die Fragen. Wenn Du fertig bist, klicke bitte auf "Weiter".',
      br(), br(),
      h3(textOutput("progress")),
      br(),
      
      h3(textOutput("Title")),
      checkboxInput("more", "Zeige Volltext.", value = F),
      conditionalPanel(
        condition = 'input.more == 1',
        fluidPage(textOutput("Text"))
      ),
      
      br(),
      br(),
      
      h3("Nimmt der Artikel Bezug auf das Thema..."),
      
      br(),
      
      column(
        4,
        
        ## migrationsfrage
        radioButtons("migration", 
                     label = "Migration und Integration?",
                     selected = "Bitte wählen",
                     choices = c("Bitte wählen", "Ja", "Nein", "Unklar")),
        
        conditionalPanel(
          condition = 'input.migration == "Unklar"',
          textInput("mig_unklar", 
                    label = "Weshalb unklar/über welches Thema spricht der Artikel?")
        ),
        
        
        ### extended definition
        checkboxInput("mig_def", "Zeige erweiterte Definition.", value = F),
        conditionalPanel(
          condition = 'input.mig_def == 1',
          fluidPage(p("Hierzu zählen bspw. Flüchtlingszahlen, Diskussionen um Zuwanderungs- und Einbürgerungsrechte, allgemeine Berichte über Migration."))
        ),
        
        ### specific migration category
        conditionalPanel(
          condition = 'input.migration == "Ja" || input.migration == "Unklar" ',
          radioButtons("mig_fine", 
                       label = "Welcher thematischen Subkategorie kann der Artikel zugeordnet werden?",
                       selected = "Bitte wählen",
                       choices = c("Bitte wählen",
                                   "Immigration (inkl. Grenzsicherung, Rückführungsdebatten, Asylrecht)", 
                                   "Integration (inkl. Einbürgerungsdebatten)", 
                                   "Sonstiges...")),
        ),
        
        ### open category
        conditionalPanel(
          condition = 'input.mig_fine == "Sonstiges..."',
          textInput("mig_oth", 
                    label = "Welcher thematischen Subkategorie kann der Artikel zugeordnet werden (Einordnung selbständig vornehmen)?")
        ),
        
        ### specific migration region
        conditionalPanel(
          condition = 'input.migration == "Ja" || input.migration == "Unklar"',
          radioButtons("mig_geo_de",
                       label = "Bezieht sich der Artikel auf Deutschland oder deutsche Politik?",
                       selected = "Bitte wählen",
                       choices = c("Bitte wählen","Ja", "Nein", "Unklar")),
        ),
        
        ### specific migration region
        conditionalPanel(
          condition = 'input.migration == "Ja" || input.migration == "Unklar"',
          radioButtons("mig_geo_eu",
                       label = "Bezieht sich der Artikel auf Europa oder Politik der EU?",
                       selected = "Bitte wählen",
                       choices = c("Bitte wählen","Ja", "Nein", "Unklar")),
        ),
        
        
        ### extended definition
        conditionalPanel(
          condition = 'input.migration == "Ja" || input.migration == "Unklar"',
          checkboxInput("mig_geo_def", "Zeige erweiterte Definition.", value = F),
        ),
        conditionalPanel(
          condition = 'input.mig_geo_def == 1',
          fluidPage("Diese Kategorie bezieht sich auf die Migration in die EU und Migrationspolitik der EU. Politik und Migration in anderen Mitgliedsstaaten ist hiervon ausgenommen. So ist bspw. eine Anlandung Geflüchteter in Italien Migration IN die EU, aber der Versuch Geflüchteter, von Calais nach Dover zu gelangen, nicht - solange nicht auf EU Ebene diskutiert.")
        )
        
      ),
      column(
        4,
        radioButtons("laworder", 
                     label = "Kriminalität, Law and Order, öffentliche Sicherheit?",
                     selected = "Bitte wählen",
                     choices = c("Bitte wählen", "Ja", "Nein", "Unklar")),
        checkboxInput("law_def", "Zeige erweiterte Definition.", value = F),
        conditionalPanel(
          condition = 'input.law_def == 1',
          fluidPage("Hierzu zählen bspw. Kriminalitätsstatistiken, Berichte über die Polizei und andere Sicherheitsorgane, sowie allgemeine Berichte über Kriminalität.")
        ),
        conditionalPanel(
          condition = 'input.laworder == "Unklar"',
          textInput("laworder_unklar", 
                    label = "Weshalb unklar/über welches Thema spricht der Artikel?")
        ),
        
        
        br(),
        radioButtons("climate", 
                     label = "Klimapolitik/Umweltpolitik?",
                     selected = "Bitte wählen",
                     choices = c("Bitte wählen", "Ja", "Nein", "Unklar")),
        checkboxInput("climate_def", "Zeige erweiterte Definition.", value = F),
        conditionalPanel(
          condition = 'input.climate_def == 1',
          fluidPage("Hierzu zählt lediglich Klimapolitik (also z.B. Diskussion einer Kerosinsteuer), keine allgemeinen Beiträge zum Thema Klimawandel.")
        ),
        conditionalPanel(
          condition = 'input.climate == "Unklar"',
          textInput("climate_unklar", 
                    label = "Weshalb unklar/über welches Thema spricht der Artikel?")
        )
        
      ),
      
      column(
        4,
        
        br(),
        radioButtons("social", 
                     label = "Deutsches Sozialsystem?",
                     selected = "Bitte wählen",
                     choices = c("Bitte wählen", "Ja", "Nein", "Unklar")),
        checkboxInput("social_def", "Zeige erweiterte Definition.", value = F),
        conditionalPanel(
          condition = 'input.social_def == 1',
          fluidPage("Hierzu zählt der gesamte Sozialstaat, z.B. auch Leistungen im Gesundheitssystem. Diskussion um Steuern und/oder die deutsche Wirtschaft sind nur zu inkludieren wenn sie sich direkt auf solche Leistungen beziehen.")
        ),
        conditionalPanel(
          condition = 'input.social == "Unklar"',
          textInput("social_unklar", 
                    label = "Weshalb unklar/über welches Thema spricht der Artikel?")
        ),
        
        
        
        br(),
        radioButtons("afd", 
                     label = "Rechtsradikalismus?",
                     selected = "Bitte wählen",
                     choices = c("Bitte wählen", "Ja", "Nein", "Unklar")),
        conditionalPanel(
          condition = 'input.afd == "Unklar"',
          textInput("afd_unklar", 
                    label = "Weshalb unklar/über welches Thema spricht der Artikel?")),
        checkboxInput("afd_def", "Zeige erweiterte Definition.", value = F),
        conditionalPanel(
          condition = 'input.afd_def == 1',
          fluidPage("Hierzu zählen sowohl Artikel über rechtsradikale Parteien, wie beispielsweise die AfD, aber auch über rechtsradikalen Terror, xenophobe Angriffe/Anschläge, migrationskritische/-feindliche Demonstrationen oder Gruppierungen. Die Ledigliche Nennung ist nicht ausreichend.")
        ),
        conditionalPanel(
          condition = 'input.afd == "Ja" || input.afd == "Unklar" ',
          radioButtons("afd_fine", 
                       label = "Welcher thematischen Subkategorie kann der Artikel zugeordnet werden?",
                       selected = "Bitte wählen",
                       choices = c("Bitte wählen",
                                   "Bericht über rechtsradikale Organisation",
                                   "Xenophobe Angriffe/Anschläge",
                                   "Rassismus/Fremdenfeindlichkeit allgemein",
                                   "Migrationsfeindliche/-kritische Demonstrationen/Proteste",
                                   "Sonstiges..."))
        ),
        ### open category
        conditionalPanel(
          condition = 'input.afd_fine == "Sonstiges..."',
          textInput("afd_oth", 
                    label = "Welcher thematischen Subkategorie kann der Artikel zugeordnet werden (Einordnung selbständig vornehmen)?")
        ),
        
        actionButton("setToNo", "Keine der Kategorien genannt")
        
      )
    ),
    
    div(),
    
    
    conditionalPanel(
      condition = "(input.migration === 'Nein' || ((input.migration === 'Ja' || input.migration === 'Unklar') && input.mig_fine != 'Bitte wählen' && input.mig_geo != 'Bitte wählen'))  && input.laworder != 'Bitte wählen' && input.climate != 'Bitte wählen' && input.social != 'Bitte wählen' && input.afd != 'Bitte wählen'",
      actionButton("nextpage", "Weiter", icon = icon("check"))
    ),
    
    conditionalPanel(
      condition = "input.nextpage > 0",
      actionButton("back", "Zurück" , icon = icon("reply"))
    ),
    
    br(),
    
    # add "save button"
    actionButton("save", "Daten speichern.", icon = icon("download")),
  )
)




# Define server logic required to draw a histogram
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
    
    dta$mig_cat_oe[is.na(dta$mig_cat_oe)]           <<- "" # ensures correct data format when importing
    dta$mig_geo[is.na(dta$mig_geo)]                 <<- "" # ensures correct data format when importing
    dta$mig_unklar[is.na(dta$mig_unklar)]           <<- "" # ensures correct data format when importing
    dta$laworder_unklar[is.na(dta$laworder_unklar)] <<- "" # ensures correct data format when importing
    dta$climate_unklar[is.na(dta$climate_unklar)]   <<- "" # ensures correct data format when importing
    dta$social_unklar[is.na(dta$social_unklar)]     <<- "" # ensures correct data format when importing
    dta$afd_cat_oe[is.na(dta$afd_cat_oe)]           <<- "" # ensures correct data format when importing
    dta$afd_unklar[is.na(dta$afd_unklar)]           <<- "" # ensures correct data format when importing
    dta$afd_oth[is.na(dta$afd_oth)]                 <<- "" # ensures correct data format when importing
    
    # define initial text
    i <<- (1 + nrow(dta) - sum(dta$mig == "")) # starting point  = 1 + number of rows - number of uncoded rows 
    rowsToCode <<- sum(dta$mig == "")
    
    output$Title <- renderText(as.character(dta$title[i]))
    output$Text <- renderText(as.character(dta$text[i]))
    
    removeModal()
    
  })
  
  
  observeEvent(input$newstart, {
    
    # import sample
    dta <<- fread("sample_handcoding.csv", encoding = "UTF-8")[, c("title", "url", "text")]
    dta <<- sample_n(dta, nrow(dta))  # reorder rows
    dta$text <<- sapply(dta[,"text"], gsub, pattern = '\"', "") # fixes issue with duplication of quotation marks
    dta$title <<- sapply(dta[,"title"], gsub, pattern = '\"', replacement = "") # fixes issue with duplication of quotation marks
    
    # define initial text
    i <<- 1
    rowsToCode <<- nrow(dta)
    
    output$Title <- renderText(as.character(dta$title[i]))
    output$Text <- renderText(as.character(dta$text[i]))
    
    dta$mig             <<- NA_character_
    dta$mig_cat         <<- NA_character_
    dta$mig_cat_oe      <<- NA_character_
    dta$mig_geo         <<- NA_character_
    dta$mig_unklar      <<- NA_character_
    dta$laworder        <<- NA_character_
    dta$laworder_unklar <<- NA_character_
    dta$climate         <<- NA_character_
    dta$climate_unklar  <<- NA_character_
    dta$social          <<- NA_character_
    dta$social_unklar   <<- NA_character_
    dta$afd             <<- NA_character_
    dta$afd_unklar      <<- NA_character_
    
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
      title="Daten gespeichert, Du kannst den Browser jetzt schliessen",
    ))
    
    Sys.sleep(5)
    
    stopApp()
    
  })
  
  observeEvent(input$nextpage, {
    
    
    # hide text
    updateCheckboxInput(session, inputId = "more", value = F)
    
    # write changes
    dta[i, "mig"]             <<- input$migration
    dta[i, "mig_cat"]         <<- input$mig_fine
    dta[i, "mig_cat_oe"]      <<- input$mig_oth
    dta[i, "mig_geo"]         <<- ifelse(length(input$mig_geo) == 0, "", input$mig_geo)
    dta[i, "mig_unklar"]      <<- input$mig_unklar
    dta[i, "laworder"]        <<- input$laworder
    dta[i, "laworder_unklar"] <<- input$laworder_unklar
    dta[i, "climate"]         <<- input$climate
    dta[i, "climate_unklar"]  <<- input$climate_unklar
    dta[i, "social"]          <<- input$social
    dta[i, "social_unklar"]   <<- input$social_unklar
    dta[i, "afd"]             <<- input$afd
    dta[i, "afd_cat"]         <<- input$afd_fine
    dta[i, "afd_cat_oe"]      <<- input$afd_oth
    dta[i, "afd_unklar"]      <<- input$afd_unklar
    
    if (i < nrow(dta)){
      
      # call next row
      i <<- i + 1
      output$Title <- renderText(as.character(dta$title[i]))
      output$Text <- renderText(as.character(dta$text[i]))
      
      # Update radiobuttons
      for (x in c("migration", "mig_fine", "mig_geo", "mig_geo_de", "mig_geo_eu", 
                  "laworder", "climate", "social", "afd", "afd_fine")){
        updateRadioButtons(session = session, inputId =  x, selected = "Bitte wählen")
      }
      
      # Update textfields
      for (x in c("mig_unklar", "mig_oth", "mig_geo", "laworder_unklar", 
                  "climate_unklar", "social_unklar", "afd_unklar", "afd_oth")){
        updateTextInput(session = session, inputId = x, value = "")
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
  
  observeEvent(input$setToNo, {
    for (x in c("migration", "laworder", "climate", "social", "afd")){
      updateRadioButtons(session = session, inputId =  x, selected = "Nein")
    }
  })
  
  observeEvent(input$back, {
    i <<- i - 1
    output$Title <- renderText(as.character(dta$title[i]))
    output$Text <- renderText(as.character(dta$text[i]))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

