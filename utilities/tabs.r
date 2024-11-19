# Tabs for translation
# This is an unelegant workaround because shiny navbars doesn't allow dynamic updates of the contents


tab1_ger <- nav_panel(
    title = "Berechnung",
    grid_card(area = "welcome", class = "grid-card-empty", card_body(uiOutput(outputId = "welcome_text"))),
    grid_container(
        layout = "
                |0rem  |300px     |300px     |400px        |300px          |
                |------|----------|----------|-------------|-------------|
                |160px |sidebar_1 |sidebar_2 |warning_text |warning_text |
                |400px |sidebar_1 |sidebar_2 |donutplot    |result_text  |
                |auto |sidebar_1 |sidebar_2  |cluster_info |cluster_info |
                |44px  |button    |button    |.            |.            |
",
        grid_card(
            area = "sidebar_1",
            class = "grid-card-left",
            radioGroupButtons(
                inputId = "relation_input", label = HTML("<strong>Verwandtschaft 1. Grades mit Typ 2 DiabetikerIn?</strong>"),
                choices = list("Nein" = 0, "Ja" = 1), justified = TRUE
            ),
            tags$hr(class = "spacer-line"),
            radioGroupButtons(
                inputId = "sex_input", label = HTML("<strong>Geschlecht:</strong>"),
                choices = list("Männlich" = 1, "Weiblich" = 0), justified = TRUE
            ),
            tags$hr(class = "spacer-line"),
            numericInput("hip_input", tags$span(style = "font-weight: bold;", "Hüftumfang (cm):"), NULL, min = 1),
            tags$script(
                # these timeouts were necessary to make the label update properly
                # I am not sure if I really need all of them (probably one is enough), so maybe test that at a later point
                # when they werent there, the label wouldn't change language
                # there was some stackoverflow I pulled this from.
                "
        $('#hip_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#hip_input-label').html($('#hip_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("waist_input", tags$span(style = "font-weight: bold;", "Taillenumfang (cm):"), NULL, min = 1),
            tags$script(
                "
        $('#waist_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#waist_input-label').html($('#waist_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("insulin_0_input", tags$span(style = "font-weight: bold;", "Insulin Nüchtern:"), NULL, min = 1),
            tags$script(
                "
        $('#insulin_0_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#insulin_0_input-label').html($('#insulin_0_input-label').text())}, 0.1);
        });
        "
            ),
            numericInput("insulin_120_input", label = HTML("<strong>Insulin 120min (OGTT):</strong>"), value = NULL, min = 1),
            awesomeRadio(
                inputId = "insulin_unit",
                label = NULL,
                choices = c("pmol/l", "mIU/l"),
                selected = "pmol/l",
                inline = TRUE,
                checkbox = TRUE
            ),
            tags$hr(class = "spacer-line"),
            numericInput("hdl_input", label = HTML("<strong>HDL:</strong>"), value = NULL, min = 1),
            awesomeRadio(
                inputId = "hdl_unit",
                label = NULL,
                choices = c("mg/dl", "mmol/l"),
                selected = "mg/dl",
                inline = TRUE,
                checkbox = TRUE
            )
        ),
        grid_card(
            area = "sidebar_2",
            class = "grid-card-right",
            radioGroupButtons(
                inputId = "gesdiab_input", label = HTML("<strong>Hatten Sie jemals Schwangerschaftsdiabetes?</strong>"),
                choices = list("Nein" = 0, "Ja" = 1), justified = TRUE
            ),
            tags$hr(class = "spacer-line"),
            numericInput("hba1c_input", label = HTML("<strong>HbA1c (%):</strong>"), value = NULL, step = 0.1, min = 1),
            tags$hr(class = "spacer-line"),
            numericInput("weight_input", tags$span(style = "font-weight: bold;", "Gewicht (kg)"), NULL, min = 1),
            tags$script(
                "
        $('#weight_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#weight_input-label').html($('#weight_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("height_input", tags$span(style = "font-weight: bold;", "Größe (cm):"), NULL, min = 1),
            tags$script(
                "
        $('#height_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#height_input-label').html($('#height_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("glucose_0_input", tags$span(style = "font-weight: bold;", "Blutzucker Nüchtern:"), NULL, min = 1),
            tags$script(
                "
        $('#glucose_0_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#glucose_0_input-label').html($('#glucose_0_input-label').text())}, 0.1);
        });
        "
            ),
            numericInput("glucose_120_input", tags$span(style = "font-weight: bold;", "Blutzucker 120min (OGTT):"), NULL, min = 1),
            tags$script(
                "
        $('#glucose_120_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#glucose_120_input-label').html($('#glucose_120_input-label').text())}, 0.1);
        });
        "
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
            numericInput("tg_input", tags$span(style = "font-weight: bold;", "Triglyceride:"), NULL, min = 1),
            tags$script(
                "
        $('#tg_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#tg_input-label').html($('#tg_input-label').text())}, 0.1);
        });
        "
            ),
            awesomeRadio(
                inputId = "tg_unit",
                label = NULL,
                choices = c("mg/dl", "mmol/l"),
                selected = "mg/dl",
                inline = TRUE,
                checkbox = TRUE
            )
        ),
        grid_card(
            class = "grid-card-button",
            area = "button",
            card_body(padding = "0px", actionButton("calculate", "Berechnen", height = "100%"))
        ),
        grid_card("warning_text", class = "markdown-separator", card_body(uiOutput(outputId = "warning_text"))),
        grid_card("cluster_info", class = "grid-card-clusterinfo", card_body(uiOutput(outputId = "cluster_info"))),
        grid_card(
            class = "grid-card-donutplot",
            area = "donutplot",
            card_body(plotOutput(outputId = "donut_output"))
        ),
        grid_card("result_text", class = "grid-card-result", card_body(uiOutput(outputId = "result_text")))
    )
)


tab1_eng <- nav_panel(
    title = "Calculation",
    grid_card(area = "welcome", class = "grid-card-empty", card_body(uiOutput(outputId = "welcome_text"))),
    grid_container(
        layout = "
                |0rem  |280px     |280px     |400px        |1fr          |
                |------|----------|----------|-------------|-------------|
                |160px |sidebar_1 |sidebar_2 |warning_text |warning_text |
                |400px |sidebar_1 |sidebar_2 |donutplot    |result_text  |
                |350px |sidebar_1 |sidebar_2 |cluster_info |cluster_info |
                |44px  |button    |button    |.            |.            |
",
        grid_card(
            area = "sidebar_1",
            class = "grid-card-left",
            radioGroupButtons(
                inputId = "relation_input", label = HTML("<strong>Verwandtschaft 1. Grades mit Typ 2 DiabetikerIn?</strong>"),
                choices = list("Nein" = 0, "Ja" = 1), justified = TRUE
            ),
            tags$hr(class = "spacer-line"),
            radioGroupButtons(
                inputId = "sex_input", label = HTML("<strong>Geschlecht:</strong>"),
                choices = list("Männlich" = 1, "Weiblich" = 0), justified = TRUE
            ),
            tags$hr(class = "spacer-line"),
            numericInput("hip_input", tags$span(style = "font-weight: bold;", "Hüftumfang (cm):"), NULL, min = 1),
            tags$script(
                # these timeouts were necessary to make the label update properly
                # I am not sure if I really need all of them (probably one is enough), so maybe test that at a later point
                # when they werent there, the label wouldn't change language
                # there was some stackoverflow I pulled this from.
                "
        $('#hip_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#hip_input-label').html($('#hip_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("waist_input", tags$span(style = "font-weight: bold;", "Taillenumfang (cm):"), NULL, min = 1),
            tags$script(
                "
        $('#waist_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#waist_input-label').html($('#waist_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("insulin_0_input", tags$span(style = "font-weight: bold;", "Insulin Nüchtern:"), NULL, min = 1),
            tags$script(
                "
        $('#insulin_0_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#insulin_0_input-label').html($('#insulin_0_input-label').text())}, 0.1);
        });
        "
            ),
            numericInput("insulin_120_input", label = HTML("<strong>Insulin 120min (OGTT):</strong>"), value = NULL),
            awesomeRadio(
                inputId = "insulin_unit",
                label = NULL,
                choices = c("pmol/l", "mIU/l"),
                selected = "pmol/l",
                inline = TRUE,
                checkbox = TRUE
            ),
            # div(style = "height: 10px;"),
            tags$hr(class = "spacer-line"),
            numericInput("hdl_input", label = HTML("<strong>HDL:</strong>"), value = NULL, min = 1),
            awesomeRadio(
                inputId = "hdl_unit",
                label = NULL,
                choices = c("mg/dl", "mmol/l"),
                selected = "mg/dl",
                inline = TRUE,
                checkbox = TRUE
            )
        ),
        grid_card(
            area = "sidebar_2",
            class = "grid-card-right",
            radioGroupButtons(
                inputId = "gesdiab_input", label = HTML("<strong>Hatten Sie jemals Schwangerschaftsdiabetes?</strong>"),
                choices = list("Nein" = 0, "Ja" = 1), justified = TRUE
            ),
            tags$hr(class = "spacer-line"),
            numericInput("hba1c_input", label = HTML("<strong>HbA1c (%):</strong>"), value = NULL, step = 0.1, min = 1),
            tags$hr(class = "spacer-line"),
            numericInput("weight_input", tags$span(style = "font-weight: bold;", "Gewicht (kg)"), NULL, min = 1),
            tags$script(
                "
        $('#weight_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#weight_input-label').html($('#weight_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("height_input", tags$span(style = "font-weight: bold;", "Größe (cm):"), NULL, min = 1),
            tags$script(
                "
        $('#height_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#height_input-label').html($('#height_input-label').text())}, 0.1);
        });
        "
            ),
            tags$hr(class = "spacer-line"),
            numericInput("glucose_0_input", tags$span(style = "font-weight: bold;", "Blutzucker Nüchtern:"), NULL, min = 1),
            tags$script(
                "
        $('#glucose_0_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#glucose_0_input-label').html($('#glucose_0_input-label').text())}, 0.1);
        });
        "
            ),
            numericInput("glucose_120_input", tags$span(style = "font-weight: bold;", "Blutzucker 120min (OGTT):"), NULL, min = 1),
            tags$script(
                "
        $('#glucose_120_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#glucose_120_input-label').html($('#glucose_120_input-label').text())}, 0.1);
        });
        "
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
            numericInput("tg_input", tags$span(style = "font-weight: bold;", "Triglyceride:"), NULL, min = 1),
            tags$script(
                "
        $('#tg_input').on('shiny:updateinput', function(e){
            setTimeout(function(){$('#tg_input-label').html($('#tg_input-label').text())}, 0.1);
        });
        "
            ),
            awesomeRadio(
                inputId = "tg_unit",
                label = NULL,
                choices = c("mg/dl", "mmol/l"),
                selected = "mg/dl",
                inline = TRUE,
                checkbox = TRUE
            )
        ),
        grid_card(
            class = "grid-card-button",
            area = "button",
            card_body(padding = "0px", actionButton("calculate", "Submit", height = "100%"))
        ),
        grid_card("warning_text", class = "markdown-separator", card_body(uiOutput(outputId = "warning_text"))),
        grid_card("cluster_info", class = "grid-card-clusterinfo", card_body(uiOutput(outputId = "cluster_info"))),
        grid_card(
            class = "grid-card-donutplot",
            area = "donutplot",
            card_body(plotOutput(outputId = "donut_output"))
        ),
        grid_card("result_text", class = "grid-card-result", card_body(uiOutput(outputId = "result_text")))
    )
)

tab2_ger <- nav_panel(
    title = "Für Forschende",
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
                fileInput("file1", NULL, accept = c(".tsv", ".txt", ".csv", ".xlsx"), width = "400px", buttonLabel = "Durchsuchen...", placeholder = "Keine Datei ausgewählt")
                # ui output for download button so we can add in dummy button
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
            card_body(padding = "0px", downloadButton("download_example", "Beispieldateien Herunterladen"), class = "batch-upload_example-button")
        )
    ),
    grid_card(area = "batch_upload_3", class = "grid-card-empty", card_body(uiOutput(outputId = "batch_upload_3"))),
)


tab2_eng <- nav_panel(
    title = "For Researchers",
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
                # ui output for download button so we can add in dummy button
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
    grid_card(area = "batch_upload_3", class = "grid-card-empty", card_body(uiOutput(outputId = "batch_upload_3"))),
)

tab3_ger <- nav_panel(
            title = "Über",
            grid_card(area = "about", class = "grid-card-empty", card_body(uiOutput(outputId = "about"), class = "about-text"), card_image("./supplementary/figures/subclusters.jpg", fill = FALSE, class = "about-image")),
        )

tab3_eng <- nav_panel(
            title = "About",
            grid_card(area = "about", class = "grid-card-empty", card_body(uiOutput(outputId = "about"), class = "about-text"), card_image("./supplementary/figures/subclusters_en.jpg", fill = FALSE, class = "about-image")),
        )
