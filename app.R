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
library(glmnet) # duplicated load
library(shinyalert)

# Load text content and dictionary for multilingual UI
text <- fromJSON("supplementary/markdowns.json")
dictionary <- fromJSON("supplementary/dictionary.json")

# Load the pre-trained LASSO model and extract variable names
model <- readRDS("lasso_model.rds")
variables <- row.names((coef(model)[[1]]))[2:length(row.names((coef(model)[[1]])))]

# Define the UI with a navbar layout
ui <- fluidPage(
  uiOutput("navbar_ui"),
  tags$link(href = "prediabclusters.css", rel = "stylesheet", type = "text/css"),
  theme = bs_theme(preset = "flatly", font_scale = 1.1)
)

server <- function(input, output, session) {
  # Store currently selected language and active tab
  current_lang <- reactiveVal("GER")
  active_tab <- reactiveVal("tab1")

  # Update language on user selection
  observeEvent(input$lang, { current_lang(input$lang) })

  # Update active tab
  observe({ active_tab(input$tabs) })

  # Dynamic UI for the navbar, including all panels and layout
  output$navbar_ui <- renderUI({
    lang <- current_lang()

    page_navbar(
      title = NULL,
      id = "tabs",
      selected = active_tab(),
      collapsible = TRUE,

      # Tab for single patient input and calculation
      nav_panel(
        title = dictionary[[current_lang()]]$tab1,
        value = "tab1",
        grid_card(area = "welcome", class = "grid-card-empty", card_body(uiOutput("welcome_text"))),
        grid_container(
          id = "input_container",
          layout = "
                |0rem  |300px     |300px     |400px        |300px          |
                |------|----------|----------|-------------|---------------|
                |auto |sidebar_1 |sidebar_2 |warning_text |warning_text   |
                |auto |sidebar_1 |sidebar_2 |donutplot    |result_text    |
                |auto  |sidebar_1 |sidebar_2 |cluster_info |cluster_info   |
                |auto  |switch    |switch    |cluster_info |cluster_info   |
                |auto  |button    |button    |cluster_info |cluster_info   |
                ",

          # Input fields for lipid and HbA1c
          grid_card(
            area = "sidebar_1",
            class = "grid-card-left",
            card_body(
              numericInput("hdl_input", tags$span(style = "font-weight: bold;", "HDL:"), value = NULL, min = 1),
              numericInput("ldl_input", tags$span(style = "font-weight: bold;", "LDL:"), value = NULL, min = 1),
              numericInput("tg_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$TG), value = NULL, min = 1),
              awesomeRadio("lipid_unit", label = NULL, choices = c("mg/dl", "mmol/l"), selected = "mg/dl", inline = TRUE, checkbox = TRUE),
              tags$hr(class = "spacer-line"),
              numericInput("waist_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$Waist), NULL, min = 1),
              tags$hr(class = "spacer-line"),
              numericInput("hba1c_input", HTML("<strong>HbA1c:</strong>"), value = NULL, min = 1),
              awesomeRadio("hba1c_unit", label = NULL, choices = c("%", "mmol/mol"), selected = "%", inline = TRUE, checkbox = TRUE)
            )
          ),

          # Input fields for glucose values, sex, and age
          grid_card(
            area = "sidebar_2",
            class = "grid-card-right",
            card_body(
              numericInput("glucose_0_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_0), NULL, min = 1),
              numericInput("glucose_60_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_60), NULL, min = 1),
              numericInput("glucose_120_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$BZ_120), NULL, min = 1),
              awesomeRadio("glucose_unit", label = NULL, choices = c("mg/dl", "mmol/l"), selected = "mg/dl", inline = TRUE, checkbox = TRUE),
              tags$hr(class = "spacer-line"),
              radioGroupButtons("sex_input", label = HTML("<strong>", dictionary[[current_lang()]]$Sex, "</strong>"),
                                choiceNames = list(dictionary[[current_lang()]]$Male, dictionary[[current_lang()]]$Female),
                                choiceValues = c(1, 0), justified = TRUE),
              tags$hr(class = "spacer-line"),
              numericInput("age_input", tags$span(style = "font-weight: bold;", dictionary[[current_lang()]]$Age), NULL, min = 1)
            )
          ),

          # Action button to calculate the cluster assignment
          grid_card(area = "button", class = "grid-card-button",
                    card_body(padding = "0px", actionButton("calculate", "Berechnen", height = "100%", class = "grid-card-button-div"))),

          # Radio buttons to select ethnicity
          grid_card(area = "switch", class = "grid-card-switch",
                    card_body(padding = "0px", radioGroupButtons(
                      inputId = "ethnicity", label = NULL,
                      choices = c("Central European", "    Asian Indian    "),
                      justified = TRUE
                    ))),

          # Displaying warnings, cluster info, donut plot, and result texts
          grid_card(area = "warning_text", class = "markdown-separator", card_body(uiOutput("warning_text"))),
          grid_card(area = "cluster_info", class = "grid-card-clusterinfo", card_body(uiOutput("cluster_info"))),
          grid_card(area = "donutplot", class = "grid-card-donutplot", card_body(plotOutput("donut_output"))),
          grid_card(area = "result_text", class = "grid-card-result", card_body(uiOutput("result_text")))
        )
      ),

      # Batch upload panel for multiple patients
      nav_panel(
        title = dictionary[[current_lang()]]$batch,
        value = "batch",
        class = "batch-upload-grid",
        grid_card(area = "batch_upload_1", class = "grid-card-empty", card_body(uiOutput("batch_upload_1"))),

        # Batch upload area with file input and result visualization
        grid_nested(
          id = "batch-upload-div",
          "batch_upload",
          layout = "
                |0px  |       auto    |  300px        |100px          |
                |-----|---------------|---------------|---------------|
                |auto |output_field_1 |output_field_2 |toggle_filetype|
                |auto |bardiagramm    |bardiagramm    |bardiagramm    |
          ",
          grid_card(area = "output_field_1", class = "grid-card-empty",
                    card_body(fileInput("file1", label = NULL,
                                        accept = c(".tsv", ".txt", ".csv", ".xlsx"),
                                        width = "400px", buttonLabel = "Browse...",
                                        placeholder = "No file selected"))),
          grid_card(area = "output_field_2", class = "batch-upload-btn-download", card_body(uiOutput("downloadresults"))),
          grid_card(area = "toggle_filetype", class = "batch-upload-typeselector", card_body(uiOutput("toggle"))),
          grid_card(area = "bardiagramm", class = "grid-card-empty", card_body(plotOutput("bardiagramm", inline = TRUE)))
        ),

        grid_card(area = "batch_upload_2", class = "grid-card-empty", card_body(uiOutput("batch_upload_2"))),

        # Download example files
        grid_container(row_sizes = c("50px"), col_sizes = c("350px"), layout = c("download_example"),
                       grid_card(area = "download_example", class = "grid-card-button2",
                                 card_body(padding = "0px",
                                           downloadButton("download_example", "Download Example File"),
                                           class = "batch-upload_example-button"))),

        grid_card(area = "batch_upload_3", class = "grid-card-empty", card_body(uiOutput("batch_upload_3")))
      ),

      # About, FAQ, Impressum tabs
      nav_panel(title = dictionary[[current_lang()]]$about, value = "about",
                grid_card(area = "about", class = "grid-card-empty", card_body(uiOutput("about"), class = "about-text"))),
      nav_panel(title = dictionary[[current_lang()]]$faq, value = "faq",
                grid_card(area = "FAQ", class = "grid-card-empty", card_body(uiOutput("FAQ")))),
      nav_panel(title = dictionary[[current_lang()]]$impressum, value = "impressum",
                grid_card(area = "impressum", class = "grid-card-empty", card_body(uiOutput("impressum")))),

      # Language and dark mode toggle
      nav_spacer(),
      nav_item(selectInput("lang", label = NULL, choices = c("German" = "GER", "English" = "ENG"),
                           selected = current_lang(), width = "120px")),
      nav_item(input_dark_mode())
    )
  })

  # Render texts for different UI sections based on selected language
  output$welcome_text <- renderUI({ markdown(md = text[[current_lang()]]$welcome) })
  output$FAQ <- renderUI({ markdown(md = text[[current_lang()]]$FAQ) })
  output$impressum <- renderUI({ markdown(md = text[[current_lang()]]$impressum) })
  output$about <- renderUI({ markdown(md = text[[current_lang()]]$about) })
  output$warning_text <- renderUI({ markdown(md = text[[current_lang()]]$warning) })
  output$batch_upload_1 <- renderUI({ markdown(md = text[[current_lang()]]$batch_upload_1) })
  output$batch_upload_2 <- renderUI({ markdown(md = text[[current_lang()]]$batch_upload_2) })
  output$batch_upload_3 <- renderUI({ markdown(md = text[[current_lang()]]$batch_upload_3) })

  # When the "Berechnen" button is clicked, check inputs and predict cluster
  observeEvent(input$calculate, {
    any_input_null <- function(...) { any(sapply(list(...), is.na)) }

    # Check if any required input is missing
    inputs_check_result <- any_input_null(
      input$hba1c_input, input$glucose_0_input, input$glucose_60_input,
      input$glucose_120_input, input$hdl_input, input$ldl_input,
      input$tg_input, input$waist_input, input$age_input
    )

    if (inputs_check_result == TRUE) {
      shinyalert("Oops!", dictionary[[current_lang()]]$Error_values, type = "error")
    } else {
      # Convert units to a standardized form used by the model
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

      # Check if patient is eligible for clustering or is normal/diabetic
      if (input$hba1c_input < 5.7 & BG_0 < 5.6 & BG_120 < 7.8) {
        output$result_text <- renderUI({ markdown(md = text[[current_lang()]]$bloodsugar_normal) })
      } else if (input$hba1c_input >= 6.5 | BG_0 >= 7 | BG_120 >= 11.1) {
        output$result_text <- renderUI({ markdown(md = text[[current_lang()]]$bloodsugar_diabetes) })
      } else {
        # Predict cluster assignment if patient is in prediabetes range
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
          BG_120 = BG_120
        ) %>% select(variables)

        values[] <- lapply(values, as.numeric)
        pred <- predict(model, s = 0.001011589, newx = as.matrix(values), type = "class")

        output$result_text <- renderUI({
          text_output <- paste(text[[current_lang()]]$cluster_assignment, pred)
          markdown(md = text_output)
        })
      }
    }
  })

  # Batch upload logic: validate file, check columns, predict clusters, show bar plot
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
      data_raw <- data.table::fread(input$file1$datapath)
    }

    expected_columns <- c("PATNO", "AGE", "SEX", "BG_0", "BG_60", "BG_120", "WAIST", "HDL", "LDL", "TG", "HBA1C")
    missing_columns <<- setdiff(expected_columns, names(data_raw))

    shiny::validate(
      need(
        length(missing_columns) == 0,
        if (length(missing_columns) == 1) {
          paste0(dictionary[[current_lang()]]$missing_sin, missing_columns, dictionary[[current_lang()]]$missing_sin_end)
        } else {
          paste0(dictionary[[current_lang()]]$missing_plu, paste(missing_columns, collapse = ", "), dictionary[[current_lang()]]$missing_plu_end)
        }
      )
    )

    valid_file <<- TRUE
    data_raw$index <- c(1:nrow(data_raw))

    data <- data_raw %>%
      dplyr::select("AGE", "SEX", "BG_0", "BG_60", "BG_120", "WAIST", "HDL", "LDL", "TG", "HBA1C", "index") %>%
      drop_na()
    index <- data$index

    data_testing <- data %>% dplyr::select(-index) %>% select(variables)
    Cluster <- predict(model, s = 0.001011589, newx = as.matrix(data_testing), type = "class")

    data_int <- data.frame(Cluster, index)
    names(data_int) <- c("Cluster", "index")

    data_return <- left_join(
      data_raw %>% select("PATNO", "AGE", "SEX", "BG_0", "BG_60", "BG_120", "WAIST", "HDL", "LDL", "TG", "HBA1C", "index"),
      data_int, by = "index"
    ) %>% dplyr::select(-"index")

    data_return[] <- lapply(data_return, function(x) if (is.numeric(x)) round(x, digits = 2) else x)
    data_return$Cluster[is.na(data_return$Cluster)] <- "Missing Values"

    return(data_return)
  })

  # Bar plot of cluster counts for batch upload
  output$bardiagramm <- renderPlot(
    height = 400,
    {
      ggplot(in_data(), aes(x = factor(Cluster), fill = as.factor(Cluster))) +
        geom_bar() +
        khroma::scale_fill_muted() +
        scale_x_discrete(name = "Cluster", expand = c(0, 0)) +
        scale_y_continuous(name = dictionary[[current_lang()]]$yaxis) +
        theme_classic() +
        ggtitle(dictionary[[current_lang()]]$plottitle) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18),
          text = element_text(size = 14)
        ) +
        guides(fill = FALSE)
    }
  )

  # Allow toggling download file format if file is valid
  output$toggle <- renderUI({
    if (!is.null(input$file1) && valid_file == TRUE) {
      prettyRadioButtons("toggle_filetype", label = NULL, choices = c(".csv", ".xlsx"), choiceValues = c("csv", "xlsx"))
    }
  })

  # Show download button if file is valid
  output$downloadresults <- renderUI({
    if (!is.null(input$file1) && valid_file == TRUE) {
      downloadButton("downloadbutton", dictionary[[current_lang()]]$downloadtext)
    }
  })

  # Handle downloading of results from batch upload
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

  # Download example files
  output$download_example <- downloadHandler(
    filename = function() { "example_files.zip" },
    content = function(file) {
      file.copy("./supplementary/example_files.zip", file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
