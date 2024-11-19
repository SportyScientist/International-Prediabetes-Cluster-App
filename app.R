library(shiny)
library(shinyalert)
library(bslib)
library(gridlayout)
library(shinyWidgets)
library(jsonlite)
library(ggplot2)
library(readr)
library(tools)
library(class)
library(readxl)
library(viridis)
library(webr)
library(dplyr)
library(tidyr)
library(vroom)
library(openxlsx)
library(shinyjs)

# theming of the plot
source("utilities/theme.r")

# imports the different versions of the tabs
source("utilities/tabs.r")


text <- fromJSON("supplementary/combined.json")
dictionary <- fromJSON("supplementary/dictionary.json")


ui <- navbarPage(

    # app logo ("/www" folder mandatory)
    title = div(class = "navbar-logo", img(src = "PreDiab-Clusters_webapp_DB1.png", width = 160, height = 41), align = "top"),
    # selected = "Berechnung",
    collapsible = TRUE,
    bg = "#5b5A5A",
    id = "test",
    theme = bs_theme(primary = "#349e8e"), # this used to be 37d1bb

    # call css stylesheet ("/www" folder mandatory)
    tags$head(
    tags$link(href = "prediabclusters.css", rel = "stylesheet", type = "text/css"),
    
    tags$title("PreDiab Clusters - Interactive Tool for Diabetes Research"),
    
    # Add meta description
    tags$meta(name = "description", content = "Explore and analyze prediabetes clusters using our interactive tool developed by IDM Tuebingen. Access scientific insights on Type 2 diabetes risk factors and more."),
    
    # Add keywords for search engines
    tags$meta(name = "keywords", content = "prediabetes, diabetes clusters, diabetes research, Type 2 diabetes, IDM Tuebingen, prediabetes cluster, clustering tool, Forschung, Helmholtz München, Helmholtz Munich, diabetes, risk cluster, diabetes risk cluster"),
    
    # Google site verification meta tag
    tags$meta(name = "google-site-verification", content = "IqDTxUpXZp_Al7FPJX-AaM63m1XkZp1MFUjg6-bFMZw"),
    
    # Additional Open Graph metadata for social sharing (optional)
    tags$meta(property = "og:title", content = "PreDiab Clusters - Interactive Tool for Diabetes Research"),
    tags$meta(property = "og:description", content = "Explore and analyze prediabetes clusters with our tool."),
    tags$meta(property = "og:url", content = "https://prediabclusters.idm-tuebingen.org/"),
    tags$meta(property = "og:type", content = "website")
    ),


    # extra logo top right navbar ("/www" folder mandatory)
    tags$script(HTML("var header = $('.navbar > .container-fluid');
    header.append('<div style=\"float:right\"><a href=\"https://www.helmholtz-munich.de/en/idm\" target=\"_blank\"><img src=\"IDM-DB.png\" alt=\"IDM Institute\" style=\"float:right;width:53px;height:41px;\"> </a></div>');
    console.log(header)")),

    # FAQ needs to be in this position
    # We use FAQs as an anchor for replacing
    tab1_ger,
    nav_panel(
        title = "FAQ",
        grid_card(area = "FAQ", class = "grid-card-empty", card_body(uiOutput(outputId = "FAQ")))
    ),
    tab2_ger,
    tab3_ger,
    nav_panel(
        title = "Impressum",
        grid_card(area = "impressum", class = "grid-card-empty", card_body(uiOutput(outputId = "impressum"))),
    ),
    nav_spacer(), # add spacer so that nav_item ends up on the right
    nav_item(
        align = "right",
        radioGroupButtons(
            status = "language-selector",
            inputId = "lang",
            choices = c(
                "GER",
                "ENG"
            )
        )
    )
)



server <- function(input, output, session) { # session needs to be in here for the dynamic UI elements (changing languages)
    # these outputs are displayed before tool is run

     WHtuefwh_meanssd <- read.csv("./supplementary/WHtuefwh_meanssd.csv")
     WHtueff_clu_medians <- read.csv("./supplementary/WHtueff_clu_medians.csv")


    # set seed for reproducability
    set.seed(28051996)

    # observe(print(input$currentTab))
    # probably rather remove this
    # session$onSessionEnded(function() {
    #     stopApp()
    # })

    # retrieve language variable
    language <- reactive({
        req(input$lang)
        language <- input$lang
        return(language)
    })

    counter <- 0

    # change Tab panel when language is changed
    observeEvent(input$lang, {
        counter <<- counter + 1

        # print(counter)
        if (language() == "GER" & counter > 1) {
            # always remove the "old" one first
            # luckily this doesn't throw any error when there is nothing to remove
            nav_remove("test", "Calculation", session)
            nav_remove("test", "Berechnung", session)


            nav_insert(
                "test", tab1_ger,
                select = F, target = "FAQ", position = "before" # target acts as a positional anchor!
            )
        } else if (language() == "ENG" & counter > 1) {
            nav_remove("test", "Berechnung", session)

            nav_insert(
                "test", tab1_eng,
                select = F, target = "FAQ", position = "before"
            )
        }


        if (language() == "GER" & counter > 1) {
            nav_remove("test", "For Researchers", session)

            nav_insert(
                "test", tab2_ger,
                select = FALSE, target = "FAQ", position = "after"
            )
        } else if (language() == "ENG" & counter > 1) {
            nav_remove("test", "Für Forschende", session)

            nav_insert(
                "test", tab2_eng,
                select = FALSE, target = "FAQ", position = "after"
            )
        }


        if (language() == "GER" & counter > 1) {
            nav_remove("test", "About", session)

            nav_insert(
                "test", tab3_ger,
                select = FALSE, target = "Impressum", position = "before"
            )
        } else if (language() == "ENG" & counter > 1) {
            nav_remove("test", "Über", session)

            nav_insert(
                "test", tab3_eng,
                select = FALSE, target = "Impressum", position = "before"
            )
        }

        observe({
            selected_tab <- input$test

            # Check if selected_tab is not empty (it is in the split second it takes to insert new tab)
            if (length(selected_tab) > 0) {
                # We need to tell the app which tab to use
                # sorry for the brute force here
                if (selected_tab == "Berechnung" & language() == "ENG") {
                    nav_select("test", selected = "Calculation")
                } else if (selected_tab == "Calculation" & language() == "GER") {
                    nav_select("test", selected = "Berechnung")
                } else if (selected_tab == "For Researchers" & language() == "GER") {
                    nav_select("test", selected = "Für Forschende")
                } else if (selected_tab == "Für Forschende" & language() == "ENG") {
                    nav_select("test", selected = "For Researchers")
                } else if (selected_tab == "About" & language() == "GER") {
                    nav_select("test", selected = "Über")
                } else if (selected_tab == "Über" & language() == "ENG") {
                    nav_select("test", selected = "About")
                }
            }
        })


        # update all the buttons depending on language
        updateRadioGroupButtons(session, "sex_input",
            choices = setNames(
                c(1, 0),
                c(
                    as.character(dictionary[[language()]]$Male),
                    as.character(dictionary[[language()]]$Female)
                )
            ),
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$Sex
            ) |>
                as.character()
        )

        updateRadioGroupButtons(session, "relation_input",
            choices = setNames(
                c(0, 1),
                c(
                    as.character(dictionary[[language()]]$No),
                    as.character(dictionary[[language()]]$Yes)
                )
            ),
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$Relation
            ) |>
                as.character()
        )


        updateRadioGroupButtons(session, "gesdiab_input",
            choices = setNames(
                c(0, 1),
                c(
                    as.character(dictionary[[language()]]$No),
                    as.character(dictionary[[language()]]$Yes)
                )
            ),
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$Gestational
            ) |>
                as.character()
        )

        updateNumericInput(session, "weight_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$Weight
            ) |>
                as.character()
        )

        updateNumericInput(session, "hip_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$Hip
            ) |>
                as.character()
        )

        updateNumericInput(session, "waist_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$Waist
            ) |>
                as.character()
        )

        updateNumericInput(session, "height_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$Height
            ) |>
                as.character()
        )

        updateNumericInput(session, "insulin_0_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$INS_0
            ) |>
                as.character()
        )
        updateNumericInput(session, "glucose_0_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$BZ_0
            ) |>
                as.character()
        )
        updateNumericInput(session, "glucose_120_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$BZ_120
            ) |>
                as.character()
        )

        updateNumericInput(session, "tg_input",
            label = tags$span(
                style = "font-weight: bold; ",
                dictionary[[language()]]$TG
            ) |>
                as.character()
        )
    })



    output$welcome_text <- renderUI({
        text <- text[[language()]]$welcome
        markdown(md = text)
    })

    output$about <- renderUI({
        text <- text[[language()]]$about

        markdown(md = text)
    })


    output$impressum <- renderUI({
        text <- text[[language()]]$impressum

        markdown(md = text)
    })

    output$FAQ <- renderUI({
        text <- text[[language()]]$FAQ

        markdown(md = text)
    })


    output$warning_text <- renderUI({
        text <- text[[language()]]$warning

        markdown(md = text)
    })

    output$batch_upload_1 <- renderUI({
        text <- text[[language()]]$batch_upload_1

        markdown(md = text)
    })

    output$batch_upload_2 <- renderUI({
        text <- text[[language()]]$batch_upload_2

        markdown(md = text)
    })


    output$batch_upload_3 <- renderUI({
        text <- text[[language()]]$batch_upload_3

        markdown(md = text)
    })


    output$result_text <- renderUI({
        text <- text[[language()]]$pre_run

        markdown(md = text)
    })

    output$donut_output <- renderImage(
        {
            pred <- 0
            list(src = sprintf("./supplementary/figures/Cluster%s.png", pred))
        },
        deleteFile = FALSE
    )

    output$cluster_info <- renderUI({
        text <- text[[language()]]$cluster_summary

        markdown(md = text)
    })




    # this only executes when button is pushed
    observeEvent(input$calculate, {
        any_input_null <- function(...) {
            any(sapply(list(...), is.na))
        }

        inputs_check_result <- any_input_null(
            input$hba1c_input, input$glucose_0_input, input$glucose_120_input,
            input$insulin_0_input, input$insulin_120_input, input$hdl_input, input$tg_input, input$height_input, input$weight_input,
            input$hip_input, input$waist_input
        )

        if (inputs_check_result == TRUE) {
            shinyalert("Oops!", dictionary[[language()]]$Error_values, type = "error")
        } else {
            # Fix all the units to what was used in the original data
            if (input$glucose_unit == "mg/dl") {
                BG_0 <- input$glucose_0_input / 18
                BG_120 <- input$glucose_120_input / 18
            } else {
                BG_0 <- input$glucose_0_input
                BG_120 <- input$glucose_120_input
            }


            if (input$insulin_unit == "pmol/l") {
                INS_0 <- input$insulin_0_input
                INS_120 <- input$insulin_120_input 
            } else {
                INS_0 <- input$insulin_0_input  / 6.95
                INS_120 <- input$insulin_120_input / 6.95
            }

            if (input$tg_unit == "mg/dl") {
                TG <- input$tg_input / 88.57
            } else {
                TG <- input$tg_input
            }

            if (input$hdl_unit == "mg/dl") {
                HDL <- input$hdl_input / 38.67
            } else {
                HDL <- input$hdl_input
            }

            # verify patient is elligable
            if ((input$hba1c_input < 5.7 & BG_0 < 5.6 & BG_120 < 7.8 & (input$gesdiab_input == "0" | (input$sex_input == "1" & input$gesdiab_input == "1")) & input$relation_input == "0") & input$weight_input / ((input$height_input / 100)^2) < 27) {
                output$donut_output <- renderImage(
                    {
                        pred <- 0
                        list(src = sprintf("./supplementary/figures/Cluster%s.png", pred))
                    },
                    deleteFile = FALSE
                )

                output$result_text <- renderUI({
                    text <- text[[language()]]$bloodsugar_normal


                    markdown(md = text)
                })
            } else if (input$hba1c_input >= 6.5 | BG_0 >= 7 | BG_120 >= 11.1) {
                output$donut_output <- renderImage(
                    {
                        pred <- 0
                        list(src = sprintf("./supplementary/figures/Cluster%s.png", pred))
                    },
                    deleteFile = FALSE
                )

                output$result_text <- renderUI({
                    text <- text[[language()]]$bloodsugar_diabetes

                    markdown(md = text)
                })
            } else if (input$hba1c_input >= 5.7 | BG_0 >= 5.6 | BG_120 >= 7.8 | input$weight_input / ((input$height_input / 100)^2) > 27 | (input$sex_input == "0" & input$gesdiab_input == "1") | (input$relation_input == "1")) {
                # create df with values
                values <- data.frame(
                    SEX = input$sex_input,
                    BMI = input$weight_input / ((input$height_input / 100)^2),
                    HDL = HDL,
                    WAIST = input$waist_input,
                    HIP = input$hip_input,
                    INS_0 = INS_0,
                    TG = TG, 
                    PATNO = 42
                )

                values[] <- lapply(values, as.numeric)

                #blood glucose values are stored as global variables!
                #in the indices everything is calculated in SI units (which is wrong but ces't la vie)
                #later on we need to convert to insulin in miU
                values <- values %>%
                    mutate(
                        ISI = 10000 / sqrt(BG_0 * (INS_0) * ((INS_0) + (INS_120)) / 2 * (BG_0 + BG_120) / 2),
                        SECR_STUM = 2503 + 6.476 * (INS_0) - 126.5 * BG_120 + 0.954 * (INS_120) - 293.3 * BG_0,
                        AUC_GLU_2P = 120 * ((BG_0 + BG_120) / 2), 
                        INS_0_miU = INS_0 / 6.94
                    ) %>%
                    select(SEX, AUC_GLU_2P, BMI, HDL, HIP, INS_0_miU, ISI, SECR_STUM, TG, WAIST)

                print(values)

                # adapted from the old app

                test1 <- values %>%
                    mutate(SEX = ifelse(SEX == 0, "female", "male")) %>%
                    gather(var, val, -SEX) %>%
                    left_join(WHtuefwh_meanssd) %>%
                    group_by(SEX, var) %>%
                    mutate(rescaled = scale(val, center = TUEF_mean[1], scale = TUEF_sd[1])) %>%
                    select(SEX, var, rescaled) %>%
                    spread(var, rescaled) %>%
                    filter_all(all_vars(!is.na(.))) %>%
                    ungroup()

                test <- test1 %>% select(-SEX)

                # use knn to predict cluster
                pred <- knn(WHtueff_clu_medians %>% select(names(test)),
                    test, cl = 1:6, k = 1)


                # source the proper exploded donut
                output$donut_output <- renderImage(
                    {
                        list(src = sprintf("./supplementary/figures/Cluster%s.png", pred))
                    },
                    deleteFile = FALSE
                )

                output$result_text <- renderUI({
                    text <- paste(text[[language()]]$cluster_assignment, pred)
                    markdown(md = text)
                })
            }
        }
    })

    # specify which fileformats are appropriate
    in_data <- reactive({
        valid_file <<- FALSE
        req(input$file1)

        valid <- c("txt", "tsv", "xlsx", "csv")
        ext <- file_ext(input$file1$name)

        shiny::validate(
            need(ext %in% valid, dictionary[[language()]]$Error_extension)
        )

        if (ext == "xlsx") {
            data_raw <- readxl::read_excel(input$file1$datapath)
        } else {
            # this should handle all other file formats
            data_raw <- data.table::fread(input$file1$datapath)
        }

        # Define the expected column names
        expected_columns <- c("PATNO", "SEX", "BG_0", "BG_120", "INS_0", "INS_120", "TG", "WAIST", "HDL", "HIP", "BMI")

        # Check if all expected columns are present
        missing_columns <<- setdiff(expected_columns, names(data_raw))
        # Add a validator and include missing columns in the error message
        shiny::validate(
            need(
                length(missing_columns) == 0,
                if (length(missing_columns) == 1 & language() == "GER") {
                    paste0("Nicht alle Variablen vorhanden! Es fehlt ", missing_columns, ".")
                } else if (length(missing_columns) > 1 & language() == "GER") {
                    paste0("Nicht alle Variablen vorhanden! Es fehlen ", paste(missing_columns, collapse = ", "), ".")
                } else if (length(missing_columns) == 1 & language() == "ENG") {
                    paste("Not all variables are present! ", missing_columns, " is missing.")
                } else if (length(missing_columns) > 1 & language() == "ENG") {
                    paste("Not all variables are present!", paste(missing_columns, collapse = ", "), "are missing.")
                }
            )
        )

        # turn valid_file TRUE if there are no missing columns
        valid_file <<- length(missing_columns) == 0
        data_raw$index <- c(1:nrow(data_raw)) 
        
        data <- data_raw %>%
            # only use complete cases
            mutate(
                ISI = 10000 / sqrt(BG_0 * INS_0 * (INS_0 + INS_120) / 2 * (BG_0 + BG_120) / 2),
                SECR_STUM = 2503 + 6.476 * INS_0 - 126.5 * BG_120 + 0.954 * INS_120 - 293.3 * BG_0,
                AUC_GLU_2P = 120 * ((BG_0 + BG_120) / 2), 
                INS_0_miU = INS_0 / 6.94

            ) %>%
            select(PATNO, SEX, AUC_GLU_2P, BMI, HDL, HIP, INS_0_miU, ISI, SECR_STUM, TG, WAIST, index) %>%
            drop_na() 

        data[] <- lapply(data, as.numeric)
        print(data)
        


        test1 <- data %>%
            mutate(SEX = ifelse(SEX == 0, "female", "male")) %>%
            filter(complete.cases(.)) %>%
            #distinct(PATNO, .keep_all = TRUE) %>%
            gather(var, val, -PATNO, -SEX, -index) %>%
            left_join(WHtuefwh_meanssd) %>%
            group_by(SEX, var) %>%
            mutate(rescaled = scale(val, center = TUEF_mean[1], scale = TUEF_sd[1])) %>%
            select(PATNO, SEX, index,var, rescaled) %>%
            spread(var, rescaled) %>%
            #filter_all(all_vars(!is.na(.))) %>%
            ungroup()


        test <- test1 %>% select(-PATNO, -SEX, -index)

        pred <- knn(WHtueff_clu_medians %>% select(names(test)),
            test,
            cl = 1:6, k = 1
        )

        # we want to return the original data with the cluster column appended
        # when a row had a missing value, we want to set the cluster to "Missing Values"

        test1$Cluster <- as.numeric(pred)
        data_fin <- select(test1, index, Cluster)


        data_return <- left_join(data_raw, data_fin, by = "index")

        data_return$Cluster[is.na(data_return$Cluster)] <- "Missing Values"

        data_return[] <- lapply(data_return, function(x) if (is.numeric(x)) round(x, digits = 2) else x)
        return(data_return)
    })

    # print(in_data())


    output$bardiagramm <- renderPlot(
        height = 400,
        {

            ggplot(in_data(), aes(x = factor(Cluster, levels = 1:6), fill = as.factor(Cluster))) +
                geom_bar() +
                scale_x_discrete(name = "Cluster", expand = c(0, 0)) +
                scale_y_continuous(name = dictionary[[language()]]$yaxis) +#, breaks = seq(0, as.numeric((max(table(in_data()$Cluster)))), 1)) +
                theme_new() +
                ggtitle(dictionary[[language()]]$plottitle) +
                theme(
                    plot.title = element_text(hjust = 0.5, size = 18), # Adjust the font size as needed
                    text = element_text(size = 14) # Adjust the overall text size if necessary
                ) +
                guides(fill = FALSE) +
                scale_fill_viridis_d(direction = -1)
        }
    )

    output$toggle <- renderUI({
        if (!is.null(input$file1) && valid_file == TRUE) {
            prettyRadioButtons(
                inputId = "toggle_filetype",
                label = NULL,
                choices = c(".csv", ".xlsx"), choiceValues = c("csv", "xlsx")
            )
        }
    })

    output$downloadresults <- renderUI({
        if (!is.null(input$file1) && valid_file == TRUE) {
            downloadButton("downloadbutton", dictionary[[language()]]$downloadtext)
        }
    })


    output$downloadbutton <- downloadHandler(
        filename = function() {
            ext <- if (input$toggle_filetype == ".csv") "csv" else "xlsx"
            paste0("prediabetes-cluster-", Sys.Date(), ".", ext)
        },
        content = function(file) {
            if (input$toggle_filetype == ".csv") {
                vroom::vroom_write(in_data(), file, delim=",")
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

shinyApp(ui, server)
