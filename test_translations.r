library(shiny)
library(bslib)
library(gridlayout)
library(shinyWidgets)
library(jsonlite)

text <- fromJSON("supplementary/combined.json")
dictionary <- fromJSON("supplementary/dictionary.json")


# Define UI
ui <- fluidPage(
  uiOutput("navbar_ui"),
  theme = bs_theme(preset = "flatly", font_scale = 1.1)
)


server <- function(input, output, session) {
  # Reactive values to store the selected language and active tab
  current_lang <- reactiveVal("ENG")
  active_tab <- reactiveVal("tab1")

  # Update language on selection
  observeEvent(input$lang, {
    req(input$lang)
    current_lang(input$lang)
  })

  # Update active tab
  observe({
    req(input$tabs)
    active_tab(input$tabs)
  })

  # Dynamic UI for Navbar
  output$navbar_ui <- renderUI({
    lang <- current_lang()

    page_navbar(
      title = NULL,
      id = "tabs",
      selected = active_tab(),
      collapsible = TRUE,

      # Tabs with dynamic titles
      nav_panel(
        title = dictionary[[current_lang()]]$tab1,
        value = "tab1",
        grid_card(area = "welcome", class = "grid-card-empty", card_body(uiOutput(outputId = "welcome_text"))),
        grid_container(
          layout = "
                |0rem  |300px     |300px     |400px        |300px          |
                |------|----------|----------|-------------|---------------|
                |160px |sidebar_1 |sidebar_2 |warning_text |warning_text   |
                |400px |sidebar_1 |sidebar_2 |donutplot    |result_text    |
                |auto  |sidebar_1 |sidebar_2 |cluster_info |cluster_info   |
                |44px  |button    |button    |.            |.              |
                ",
          grid_card(
            area = "sidebar_1",
            class = "grid-card-left",
            numericInput("hdl_input", label = HTML("<strong>HDL:</strong>"), value = NULL, min = 1),
            numericInput("ldl_input", label = HTML("<strong>LDL:</strong>"), value = NULL, min = 1),
            numericInput("tg_input", label = HTML("<strong>", dictionary[[current_lang()]]$TG, "</strong>"), value = NULL, min = 1),
            awesomeRadio(
              inputId = "lipid_unit",
              label = NULL,
              choices = c("mg/dl", "mmol/l"),
              selected = "mg/dl",
              inline = TRUE,
              checkbox = TRUE
            ),
            #
            tags$hr(class = "spacer-line"),
            numericInput(
              "waist_input",
              tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$Waist),
              NULL,
              min = 1
            ),
            #
            tags$hr(class = "spacer-line"),
            numericInput(
              "hba1c_input",
              label = HTML("<strong>HbA1c:</strong>"),
              value = NULL, min = 1
            ),
            #
            awesomeRadio(
              inputId = "hba1c_unit",
              label = NULL,
              choices = c("%", "mmol/mol"),
              selected = "%",
              inline = TRUE,
              checkbox = TRUE
            )
            #
          ),
          grid_card(
            area = "sidebar_2",
            class = "grid-card-right",
            numericInput(
              "glucose_0_input",
              tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_0),
              NULL,
              min = 1
            ),
            numericInput(
              "glucose_60_input",
              tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_60),
              NULL,
              min = 1
            ),
            numericInput(
              "glucose_120_input",
              tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_120),
              NULL,
              min = 1
            ),
            awesomeRadio(
              inputId = "glucose_unit",
              label = NULL,
              choices = c("mg/dl", "mmol/l"),
              selected = "mg/dl",
              inline = TRUE,
              checkbox = TRUE
             ),
            tags$hr(class = "spacer-line"),
            radioGroupButtons(
              inputId = "sex_input",
              label = HTML("<strong>", dictionary[[current_lang()]]$Sex, "</strong>"),
              choices = list(
                "Male" = as.character(dictionary[[current_lang()]]$Male),
                "Female" = as.character(dictionary[[current_lang()]]$Female)
              ),
              justified = TRUE
            ),
            #
            tags$hr(class = "spacer-line"),
            numericInput(
              "age_input",
              tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$Age),
              NULL,
              min = 1
            )
          ),
          grid_card(
            class = "grid-card-button",
            area = "button",
            card_body(padding = "0px", actionButton("calculate", "Berechnen", height = "100%"))
          ),
          grid_card(
            area = "warning_text",
            class = "markdown-separator",
            card_body(uiOutput(outputId = "warning_text"))
          ),
          grid_card(
            area = "cluster_info",
            class = "grid-card-clusterinfo",
            card_body(uiOutput(outputId = "cluster_info"))
          ),
          grid_card(
            class = "grid-card-donutplot",
            area = "donutplot",
            card_body(plotOutput(outputId = "donut_output"))
          ),
          grid_card(
            area = "result_text",
            class = "grid-card-result",
            card_body(uiOutput(outputId = "result_text"))
          )
        )
      ),
      nav_panel(
        title = dictionary[[current_lang()]]$batch,
        value = "batch",
        grid_card(
          area = "about",
          class = "grid-card-empty",
          card_body(uiOutput(outputId = "about"), class = "about-text")
        )
      ),
      nav_panel(
        title = dictionary[[current_lang()]]$about,
        value = "about",
        grid_card(
          area = "about",
          class = "grid-card-empty",
          card_body(uiOutput(outputId = "about"), class = "about-text")
        )
      ),
      nav_panel(
        title = dictionary[[current_lang()]]$faq,
        value = "faq",
        grid_card(
          area = "FAQ",
          class = "grid-card-empty",
          card_body(uiOutput(outputId = "FAQ"))
        )
      ),
      nav_panel(
        title = dictionary[[current_lang()]]$impressum,
        value = "impressum",
        grid_card(
          area = "impressum",
          class = "grid-card-empty",
          card_body(uiOutput(outputId = "impressum"))
        )
      ),
      nav_spacer(),
      nav_item(
        selectInput(
          inputId = "lang",
          label = NULL,
          choices = c("German" = "GER", "English" = "ENG"),
          selected = current_lang(),
          width = "120px"
        )
      ),
      nav_item(input_dark_mode())
    )
  })

  ## Define outputs
  output$welcome_text <- renderUI({
    text <- text[[current_lang()]]$welcome
    markdown(md = text)
  })

  output$FAQ <- renderUI({
    text <- text[[current_lang()]]$FAQ
    markdown(md = text)
  })

  output$impressum <- renderUI({
    text <- text[[current_lang()]]$impressum
    markdown(md = text)
  })

  output$about <- renderUI({
    text <- text[[current_lang()]]$about
    markdown(md = text)
  })
}

# Run the app
shinyApp(ui, server)
