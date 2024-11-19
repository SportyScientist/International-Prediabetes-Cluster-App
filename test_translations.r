library(shiny)
library(bslib)
library(gridlayout)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  # Dynamic UI for Navbar
  uiOutput("navbar_ui"), 
  theme = bs_theme(preset = "flatly", font_scale = 1.1)
)

# Define server logic
server <- function(input, output, session) {
  # Multilingual dictionary
  translations <- list(
    GER = list(
      tab1 = "Berechnung",
      tab2 = "Datenanalyse",
      tab3 = "Visualisierung",
      faq = "Häufig gestellte Fragen",
      impressum = "Impressum"
    ),
    ENG = list(
      tab1 = "Calculation",
      tab2 = "Data Analysis",
      tab3 = "Visualization",
      faq = "FAQ",
      impressum = "Legal Notice"
    )
  )
  
  # Reactive value to store the selected language
  current_lang <- reactiveVal("GER")
  
  # Observe changes to the language selector
  observe({
    req(input$lang) # Ensure input is available
    current_lang(input$lang)
  })
  
  # Dynamic UI for Navbar
  output$navbar_ui <- renderUI({
    lang <- current_lang()
    
    navbarPage(
      # App title with logo
      title = NULL,
      collapsible = TRUE,
      id = "test",
         
      # Tabs with dynamic titles
      tabPanel(translations[[lang]]$tab1, "Content for Tab 1"),
      tabPanel(translations[[lang]]$tab2, "Content for Tab 2"),
      tabPanel(translations[[lang]]$tab3, "Content for Tab 3"),
      
      # FAQ and Impressum tabs
      nav_panel(
        title = translations[[lang]]$faq,
        grid_card(area = "FAQ", class = "grid-card-empty", card_body(uiOutput(outputId = "FAQ")))
      ),
      nav_panel(
        title = translations[[lang]]$impressum,
        grid_card(area = "impressum", class = "grid-card-empty", card_body(uiOutput(outputId = "impressum")))
      ),
      
      # Spacer and Language Selector
      nav_spacer(),
      nav_item(
        selectInput(inputId = "lang", label = NULL, choices = c("German" = "GER","English" = "ENG"), selected = current_lang(), width = "120px")
      ), 
      nav_item(input_dark_mode())
    )
  })
  
  # FAQ and Impressum content
  output$FAQ <- renderUI({
    lang <- current_lang()
    if (lang == "GER") {
      "Hier finden Sie häufig gestellte Fragen."
    } else {
      "Here you can find frequently asked questions."
    }
  })
  
  output$impressum <- renderUI({
    lang <- current_lang()
    if (lang == "GER") {
      "Impressum auf Deutsch."
    } else {
      "Legal notice in English."
    }
  })
}

# Run the app
shinyApp(ui, server)
