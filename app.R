library(shiny)
library(bslib)
library(gridlayout)
library(shinyWidgets)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(xfun)
library(glmnet)
library(tidylog)
library(khroma)
library(glmnet)
library(shinyalert)



text <- fromJSON("supplementary/markdowns.json")
dictionary <- fromJSON("supplementary/dictionary.json")
model <- readRDS("lasso_model.rds")

variables <- row.names((coef(model)[[1]]))[2:length(row.names((coef(model)[[1]])))]

# Define UI
ui <- fluidPage(
  uiOutput("navbar_ui"),
  tags$link(href = "prediabclusters.css", rel = "stylesheet", type = "text/css"),
  theme = bs_theme(preset = "flatly", font_scale = 1.4)
)

server <- function(input, output, session) {
  # Reactive values to store the selected language and active tab
  current_lang <- reactiveVal("GER")
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
                |220px |sidebar_1 |sidebar_2 |warning_text |warning_text   |
                |160px |sidebar_1 |sidebar_2 |donutplot    |result_text    |
                |auto  |sidebar_1 |sidebar_2 |cluster_info |cluster_info   |
                |auto  |button    |button    |cluster_info |cluster_info   |
                ",
          grid_card(
            area = "sidebar_1",
            class = "grid-card-left",
            card_body(
              numericInput("hdl_input", tags$span(style = "font-weight: bold;", "HDL:"), value = NULL, min = 1),
              numericInput("ldl_input", tags$span(style = "font-weight: bold;", "LDL:"), value = NULL, min = 1),
              numericInput("tg_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$TG), value = NULL, min = 1),
              awesomeRadio(
                inputId = "lipid_unit",
                label = NULL,
                choices = c("mg/dl", "mmol/l"),
                selected = "mg/dl",
                inline = TRUE,
                checkbox = TRUE
              ),
              tags$hr(class = "spacer-line"),
              numericInput("waist_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$Waist), NULL, min = 1),
              tags$hr(class = "spacer-line"),
              numericInput("hba1c_input", label = HTML("<strong>HbA1c:</strong>"), value = NULL, min = 1),
              awesomeRadio(
                inputId = "hba1c_unit",
                label = NULL,
                choices = c("%", "mmol/mol"),
                selected = "%",
                inline = TRUE,
                checkbox = TRUE
              )
            )
          ),
          grid_card(
            area = "sidebar_2",
            class = "grid-card-right",
            card_body(
              numericInput("glucose_0_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_0), NULL, min = 1),
              numericInput("glucose_60_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_60), NULL, min = 1),
              numericInput("glucose_120_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_120), NULL, min = 1),
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
                choiceNames = list(
                  (dictionary[[current_lang()]]$Male),
                  (dictionary[[current_lang()]]$Female)
                ), choiceValues = c(1,0),
                justified = TRUE
              ),
              tags$hr(class = "spacer-line"),
              numericInput(
                "age_input",
                tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$Age),
                NULL,
                min = 1
              )
            )
          ),
          grid_card(
            class = "grid-card-button",
            area = "button",
                        card_body(padding = "0px", radioGroupButtons(inputId = "ethnicity", label = NULL, choices = c("Central European", 
    "    Asian Indian    "),
   justified = TRUE, disabled = TRUE)),
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
        class = "batch-upload_grid",
        grid_card(area = "batch_upload_1", class = "grid-card-empty", card_body(uiOutput(outputId = "batch_upload_1"))),
        grid_nested(
          id = "batch-upload_div",
          "batch_upload",
          layout = "
                |0rem  |   500px  |  300px   |100px |
                |------|----------|----------|----------|
                | |output_field_1 |output_field_2 |toggle_filetype|
                | |bardiagramm |bardiagramm |bardiagramm |
          ",
          grid_card(
            area = "output_field_1",
            class = "grid-card-empty",
            card_body(
              class = "batch-upload_download-button",
              fileInput("file1", NULL, accept = c(".tsv", ".txt", ".csv", ".xlsx"), width = "400px", buttonLabel = "Browse...", placeholder = "No file selected")
            )
          ),
          grid_card(area = "output_field_2", class = "grid-card-empty btn-download", card_body(uiOutput("downloadresults"))),
          grid_card(area = "toggle_filetype", class = "batch-upload_typeselector", card_body(
            uiOutput("toggle")
          )),
          grid_card(area = "bardiagramm", class = "grid-card-empty", card_body(plotOutput(outputId = "bardiagramm", inline = TRUE)))
        ),
        grid_card(area = "batch_upload_2", class = "grid-card-empty", card_body(uiOutput(outputId = "batch_upload_2"))),
        grid_container(
          row_sizes = c("50px"),
          col_sizes = c("350px"),
          layout = c("download_example"),
          grid_card(
            area = "download_example", class = "grid-card-button2",
            card_body(padding = "0px", downloadButton("download_example", "Download Example File"), class = "batch-upload_example-button")
          )
        ),
        grid_card(area = "batch_upload_3", class = "grid-card-empty", card_body(uiOutput(outputId = "batch_upload_3")))
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

  output$warning_text <- renderUI({
    text <- text[[current_lang()]]$warning
    markdown(md = text)
  })

  output$batch_upload_1 <- renderUI({
    text <- text[[current_lang()]]$batch_upload_1
    markdown(md = text)
  })

  output$batch_upload_2 <- renderUI({
    text <- text[[current_lang()]]$batch_upload_2
    markdown(md = text)
  })

  output$batch_upload_3 <- renderUI({
    text <- text[[current_lang()]]$batch_upload_3
    markdown(md = text)
  })


  ###
  # one by one input
  ###

  observeEvent(input$calculate, {
    any_input_null <- function(...) {
      any(sapply(list(...), is.na))
    }

    inputs_check_result <- any_input_null(
      input$hba1c_input, input$glucose_0_input, input$glucose_60_input, input$glucose_120_input,
      input$hdl_input, input$ldl_input, input$tg_input, input$waist_input, input$age_input
    )

    if (inputs_check_result == TRUE) {
      shinyalert("Oops!", dictionary[[current_lang()]]$Error_values, type = "error")
    } else {
      # Fix all the units to what was used in the original data
      if (input$glucose_unit == "mg/dl") {
        BG_0 <- input$glucose_0_input / 18
        BG_60 <- input$glucose_60_input / 18
        BG_120 <- input$glucose_120_input / 18
      } else {
        BG_0 <- input$glucose_0_input
        BG_60 <- input$glucose_60_input
        BG_120 <- input$glucose_120_input
      }


      if (input$lipid_unit == "mg/dl") {
        TG <- input$tg_input / 88.57
        HDL <- input$hdl_input / 38.67
        LDL <- input$ldl_input / 38.67
      } else {
        TG <- input$tg_input
        HDL <- input$hdl_input
        LDL <- input$ldl_input
      }

      if (input$hba1c_unit == "mmol/mol") {
        HBA1C <- (input$hba1c_input - 2.15) * 10.929
      } else {
        HBA1C <- input$hba1c_input
      }


      # verify patient is elligable
      if (input$hba1c_input < 5.7 & BG_0 < 5.6 & BG_120 < 7.8 ) {
      
         output$result_text <- renderUI({
          text <- text[[current_lang()]]$bloodsugar_normal
          markdown(md = text)
      
        })
      
      } else if (input$hba1c_input >= 6.5 | BG_0 >= 7 | BG_120 >= 11.1) {
       
        output$result_text <- renderUI({
          text <- text[[current_lang()]]$bloodsugar_diabetes

          markdown(md = text)
        })

      }  else if (input$hba1c_input >= 5.7 | BG_0 >= 5.6 | BG_120 >= 7.8) {

        # create df with values

        values <- data.frame(
          AGE = input$age_input,
          WAIST = input$waist_input,
          TG = TG, 
          HDL = HDL, 
          LDL = LDL, 
          HBA1C = HBA1C, 
          SEX = input$sex_input, 
          BG_0 = BG_0, 
          BG_60 = BG_60, 
          BG_120 = BG_120) %>% select(variables)

        values[] <- lapply(values, as.numeric)

        pred <- predict(model, s = 0.001011589, newx = as.matrix(values), type = "class")



        output$result_text <- renderUI({
          text <- paste(text[[current_lang()]]$cluster_assignment, pred)
          markdown(md = text)
        })
      }
    }
  })



  #######
  # Batch Upload
  #######

  # File validation
  in_data <- reactive({
    valid_file <<- FALSE
    req(input$file1)

    valid <- c("txt", "tsv", "xlsx", "csv")
    ext <- file_ext(input$file1$name)

    shiny::validate(
      need(ext %in% valid, dictionary[[current_lang()]]$Error_extension)
    )

    if (ext == "xlsx") {
      data_raw <- readxl::read_excel(input$file1$datapath)
    } else {
      # this should handle all other file formats
      data_raw <- data.table::fread(input$file1$datapath)
    }

    # Define the expected column names
    expected_columns <- c("PATNO", "AGE", "SEX", "BG_0", "BG_60", "BG_120", "WAIST", "HDL", "LDL", "TG", "HBA1C")

    # Check if all expected columns are present
    missing_columns <<- setdiff(expected_columns, names(data_raw))
    # Add a validator and include missing columns in the error message
    shiny::validate(
      need(
        length(missing_columns) == 0,
        if (length(missing_columns) == 1) {
          paste0(dictionary[[current_lang()]]$missing_sin, missing_columns, dictionary[[current_lang()]]$missing_sin_end)
        } else if (length(missing_columns) > 1) {
          paste0(dictionary[[current_lang()]]$missing_plu, paste(missing_columns, collapse = ", "), dictionary[[current_lang()]]$missing_plu_end)
        }
      )
    )

    # turn valid_file TRUE if there are no missing columns
    valid_file <<- length(missing_columns) == 0
    data_raw$index <- c(1:nrow(data_raw))


    ###
    # data_raw <- test

    data <- data_raw %>%
      dplyr::select("AGE", "SEX", "BG_0", "BG_60", "BG_120", "WAIST", "HDL", "LDL", "TG", "HBA1C", "index") %>%
      drop_na()
    index <- data$index

    data_testing <- data %>%
      dplyr::select(-index) %>%
      select("AGE", "WAIST", "TG", "HDL", "LDL", "BG_0", "BG_60", "BG_120", "HBA1C", "SEX")


    Cluster <- predict(model, s = 0.001011589, newx = as.matrix(data_testing), type = "class")

    data_int <- data.frame(Cluster, index)
    names(data_int) <- c("Cluster", "index")
    data_return <- left_join(data_raw %>% select("PATNO", "AGE", "SEX", "BG_0", "BG_60", "BG_120", "WAIST", "HDL", "LDL", "TG", "HBA1C", "index"), data_int, by = "index") %>% dplyr::select(-"index")

    data_return[] <- lapply(data_return, function(x) if (is.numeric(x)) round(x, digits = 2) else x)

    data_return$Cluster[is.na(data_return$Cluster)] <- "Missing Values"


    return(data_return)
  })

  ## add barplot
  output$bardiagramm <- renderPlot(
    height = 400,
    {
      ggplot(in_data(), aes(x = factor(Cluster), fill = as.factor(Cluster))) +
        geom_bar() +
        khroma::scale_fill_light() +
        scale_x_discrete(name = "Cluster", expand = c(0, 0)) +
        scale_y_continuous(name = dictionary[[current_lang()]]$yaxis) + # , breaks = seq(0, as.numeric((max(table(in_data()$Cluster)))), 1)) +
        theme_classic() +
        ggtitle(dictionary[[current_lang()]]$plottitle) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18), # Adjust the font size as needed
          text = element_text(size = 14) # Adjust the overall text size if necessary
        ) +
        guides(fill = FALSE)
    }
  )

  ## toggle

  output$toggle <- renderUI({
    if (!is.null(input$file1) && valid_file == TRUE) {
      prettyRadioButtons(
        inputId = "toggle_filetype",
        label = NULL,
        choices = c(".csv", ".xlsx"), choiceValues = c("csv", "xlsx")
      )
    }
  })

  ## downloadresults button

  output$downloadresults <- renderUI({
    if (!is.null(input$file1) && valid_file == TRUE) {
      downloadButton("downloadbutton", dictionary[[current_lang()]]$downloadtext)
    }
  })

  ### downloadhandler


  output$downloadbutton <- downloadHandler(
    filename = function() {
      ext <- if (input$toggle_filetype == ".csv") "csv" else "xlsx"
      paste0("international-prediabetes-cluster-", Sys.Date(), ".", ext)
    },
    content = function(file) {
      if (input$toggle_filetype == ".csv") {
        vroom::vroom_write(in_data(), file, delim = ",")
      } else {
        openxlsx::write.xlsx(in_data(), file)
      }
    }
  )


  # download example files
  output$download_example <- downloadHandler(
    filename = function() {
      "example_files.zip"
    },
    content = function(file) {
      file.copy("./supplementary/example_files.zip", file)
    }
  )
}

# Run the app
shinyApp(ui, server)
