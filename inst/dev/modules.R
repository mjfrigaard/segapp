#' import_data UI Function
#'
#' @description A shiny Module.
#'
mod_import_data_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bs4Dash::box(
      title = "Upload", width = 12,
      shiny::fluidRow(
        shiny::column(
          width = 4,
          # import a data file
          shiny::fileInput(
            inputId = ns("data_import"),
            label = "Please upload a data file",
            # plain text and excel
            accept = c(
              ".csv", ".tsv", ".txt",
              ".xlsx", ".xls"
            )
          )
        ),
        # conditional sheets for .xlsx
        shiny::column(
          width = 4,
          shiny::uiOutput(ns("cond_xlsx"))
        ),
        # shiny::fluidRow(
        shiny::column(
          width = 4,
          tags$em("Need an example?"),
          shiny::downloadButton(
            outputId = ns("download_csv"),
            label = "Sample .csv file"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::tags$strong(
            shiny::tags$em(
              "You can preview your uploaded data below:"
            )
          ),
          reactable::reactableOutput(ns("display_upload"))
        )
      )
      # reactive values (dev only) ----
      # shiny::fluidRow(
      #   shiny::column(
      #     width = 12,
      #     htmltools::code("module reactive values"),
      #     shiny::verbatimTextOutput(outputId = ns("values"))
      #   )
      # )
    )
  )
}

#' import_data Server Functions
#'
mod_import_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # download example
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste("Sample", "Data", "File", ".csv", sep = "")
      },
      content = function(file) {
        file.copy("app_sample_data.csv", file)
      },
      contentType = "text/csv"
    )

    # conditional selectInput() for xlsx files
    output$cond_xlsx <- shiny::renderUI({
      shiny::req(
        tools::file_ext(input$data_import$datapath) == "xlsx"
      )
      shiny::selectInput(
        inputId = ns("sheet"),
        label = "Please selelct a sheet",
        choices = c("", NULL),
        selected = NULL
      )
    })

    # update selectInput() with excel sheets
    shiny::observe({
      shiny::req(
        tools::file_ext(input$data_import$datapath) == "xlsx"
      )
      sheets <- readxl::excel_sheets(
        path = input$data_import$datapath
      )
      shiny::updateSelectInput(
        inputId = "sheet",
        choices =  sheets
      )
    }) |>
      # bound to the conditional 'data import'
      shiny::bindEvent(c(input$data_import))

    # reactive imported data
    imported_data <- shiny::reactive({
      if (tools::file_ext(input$data_import$datapath) == "xlsx") {
        imp_sheet <- input$sheet
        uploaded <- import_data(
          path = input$data_import$datapath,
          sheet = imp_sheet
        )
      } else {
        uploaded <- import_data(
          path = input$data_import$datapath
        )
      }
      return(uploaded)
    }) |>
      # bind to imported data and sheets
      shiny::bindEvent(c(input$data_import, input$sheet),
        ignoreNULL = FALSE
      )

    # display imported data
    shiny::observe({
      shiny::req(input$data_import)
      output$display_upload <- reactable::renderReactable(
        reactable::reactable(
          data = imported_data(),
          defaultPageSize = 5,
          bordered = TRUE,
          defaultColDef = reactable::colDef(footer = function(values) {
            if (!is.numeric(values)) return()
            sparkline::sparkline(values, type = "box",
              width = 300, height = 80)
          })
        )
      )
    }) |>
      # bind to imported data
      shiny::bindEvent(input$data_import)

    # # all reactives (dev only)
    # vals <- reactive({
    #   shiny::reactiveValuesToList(
    #     x = input,
    #     all.names = TRUE
    #   )
    # })
    #
    # # reactives (dev only)
    # output$values <- shiny::renderPrint({
    #   # remove reactable ids
    #   react_ids <- stringr::str_detect(names(vals()),
    #     "__reactable__",
    #     negate = TRUE
    #   )
    #   vals()[react_ids]
    # })

    return(
      list(
        imported_data = shiny::reactive({
          shiny::req(input$data_import)
          if (tools::file_ext(input$data_import$datapath) == "xlsx") {
            imp_sheet <- input$sheet
            uploaded <- import_data(
              path = input$data_import$datapath,
              sheet = imp_sheet
            )
          } else {
            uploaded <- import_data(
              path = input$data_import$datapath
            )
          }
          return(uploaded)
        })
      )
    )
  })
}

#' mod_pair_type UI Function
#'
#' @description A shiny Module.
mod_pair_type_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        # BGM text ----
        shiny::tags$p(
          shiny::tags$strong(
            shiny::tags$code("BGM")
          ), " = Blood Glucose Monitor"
        ),
        # REF text ----
        shiny::tags$p(
          shiny::tags$strong(
            shiny::tags$code("REF")
          ), " = Reference"
        ),
        shiny::uiOutput(ns("text_pairs"))
      ),
      shiny::column(
        width = 6,
        reactable::reactableOutput(ns("display_pairs"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}



#' mod_pair_type Server Functions
#'
mod_pair_type_server <- function(id, module_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "import a .csv file"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_pairs <- reactable::renderReactable({
        shiny::validate(
          shiny::need(imported(), "import a .csv file")
        )
        pairs_tbl <- segtools::seg_pair_type_tbl(imported())
        reactable::reactable(
          data = pairs_tbl
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    shiny::observe({
      output$text_pairs <- shiny::renderText({
        # Table description text ----
        HTML(paste0(shiny::tags$p("This contains the number of ", shiny::tags$code("BGM"), "values that were 1) less than the ", shiny::tags$code("REF"), "values, 2) equal to the ", shiny::tags$code("REF"), "values, and 3) greater than the ", shiny::tags$code("REF"), "values. Note that ", shiny::tags$code("REF"), "values >600 mg/dL will not be plotted on the SEG.")))
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    # dev purposes
    # output$values <- shiny::renderPrint({
    #   str(
    #     imported()
    #     )
    # })
  })
}

#' mod_table UI Function
#'
#' @description MARD table UI function
mod_mard_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::uiOutput(ns('text_mard'))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_mard"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' mod_mard_table Server Functions
#'
#' @description MARD table server function
#'
mod_mard_table_server <- function(id, module_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_mard <- reactable::renderReactable({
        # data for MARD table
          shiny::validate(
            shiny::need(
              expr = module_data$imported_data(),
              message = "please import data"
            )
          )
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        mard <- segtools::seg_mard_tbl(risk_vars = risk_vars_tbl)

        reactable::reactable(
          data = mard
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

      shiny::observe({
       output$text_mard <- shiny::renderText({
          shiny::validate(
            shiny::need(
              expr = module_data$imported_data(),
              message = "please import data"
            )
          )
         htmltools::HTML(
           paste0(
             shiny::tags$p(
               shiny::tags$strong("Bias:"),
               shiny::tags$em("Mean relative difference between BGM and REF"),
               shiny::tags$code(shiny::tags$strong(" (BGM-REF ) / REF "))
             ),
             shiny::tags$p(
               shiny::tags$strong("MARD:"),
               shiny::tags$em("Mean Absolute Relative Difference"),
               shiny::tags$code(shiny::tags$strong(" | BGM-REF | / REF "))
             ),
             shiny::tags$p(
               shiny::tags$strong("CV:"),
               shiny::tags$em("Standard Deviation of Relative Difference between BGM and REF")
             ),
             shiny::tags$p(
               shiny::tags$strong("Lower 95% Limit of Agreement:"),
               shiny::tags$code(shiny::tags$strong("Bias - 1.96 * CV"))
             ),
             shiny::tags$p(
               shiny::tags$strong("Upper 95% Limit of Agreement:"),
               shiny::tags$code(shiny::tags$strong("Bias + 1.96 * CV"))
             )
           )
         )
       })
     }) |>
       # bind this to import
       shiny::bindEvent(imported())


  })
}

#' risk_grade UI Function
#'
#' @description Risk grade shiny UI module.
mod_risk_grade_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_grade"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' risk_grade Server Functions
#'
#' @description Risk grade table server function
mod_risk_grade_server <- function(id, module_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_grade <- reactable::renderReactable({
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        risk_grade_tbl <- segtools::seg_risk_grade_tbl(risk_vars = risk_vars_tbl)
        reactable::reactable(
          data = risk_grade_tbl,
          compact = TRUE,
          rowStyle = function(index) {
            if (risk_grade_tbl[index, "Risk Grade"] == "A") {
              list(
                background = "#33CC33",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "B") {
              list(
                background = "#99FF33",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "C") {
              list(
                background = "#FFFF00",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "D") {
              list(
                background = "#FF9900",
                fontWeight = "bold"
              )
            } else if (risk_grade_tbl[index, "Risk Grade"] == "E") {
              list(
                background = "#FF0000",
                fontWeight = "bold"
              )
            }
          }
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())
  })
}

#' risk_level UI Function
#'
#' @description risk level shiny UI module.
#'
mod_risk_category_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_level"))
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' risk_level Server Functions
#'
#' @description Risk level table server function
mod_risk_category_server <- function(id, module_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_level <- reactable::renderReactable({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        risk_cat_tbl <- segtools::seg_risk_cat_tbl(risk_vars = risk_vars_tbl)
        reactable::reactable(
          data = risk_cat_tbl,
          compact = TRUE,
          rowStyle = function(index) {
            if (risk_cat_tbl[index, "SEG Risk Category"] == "None") {
              list(
                background = "#00EE00",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Slight, Lower") {
              list(
                background = "#ADFF2F",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Slight, Higher") {
              list(
                background = "#FFFF00",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Moderate, Lower") {
              list(
                background = "#FFD700",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Moderate, Higher") {
              list(
                background = "#FFA500",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Severe, Lower") {
              list(
                background = "#EE7600",
                fontWeight = "bold"
              )
            } else if (risk_cat_tbl[index, "SEG Risk Category"] == "Severe, Upper") {
              list(
                background = "#FF4500",
                fontWeight = "bold"
              )
            } else {
              list(
                background = "#FF0000",
                fontWeight = "bold"
              )
            }
          }
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())
  })
}

#' iso_range UI Function
#'
#' @description A shiny Module
mod_iso_range_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_iso_range")))),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::tags$br(),
        shiny::uiOutput(ns("text_iso_range"))
      )
    )
  )
}

#' iso_range Server Functions
#'
mod_iso_range_server <- function(id, module_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_iso_range <- reactable::renderReactable({
        risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
        iso_ranges <- segtools::seg_iso_range_tbl(risk_vars_tbl)
        reactable::reactable(
          data = iso_ranges
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    shiny::observe({
      output$text_iso_range <- shiny::renderText({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        htmltools::HTML(
          paste0(
            shiny::tags$p(
              shiny::tags$em(
                "ISO range = difference between BGM and REF as percent of REF for REF > 100 mg/dL and in mg/dL for REF <= 100 mg/dL."
              )
            )
          )
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())
  })
}

#' compliant_pairs UI Function
#'
#' @description A shiny Module.
#'
mod_compliant_pairs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 12,
        shiny::uiOutput(ns('text_compliant_pairs'))
        )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        reactable::reactableOutput(ns("display_compliant_pairs")),
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns('text_source'))
      )
    )

  )
}

#' compliant_pairs Server Functions
#'
mod_compliant_pairs_server <- function(id, module_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    shiny::observe({
      output$text_compliant_pairs <- shiny::renderText({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        htmltools::HTML(
          paste0(
            shiny::tags$p(
              "Models indicate that a device with ≥ 97% pairs inside the SEG no-risk 'green' zone would meet the requirements of ≤ 5% data pairs outside the 15 mg/dL (0.83 mmol/L) / 15% standard limits, while higher percentages outside the SEG no-risk zone would indicate noncompliance with the standard."
            )
          )
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    # use observe for the side-effect here
    shiny::observe({
      output$display_compliant_pairs <- reactable::renderReactable({
            risk_vars_tbl <- segtools::seg_risk_vars(df = imported())
            binom_table <-  segtools::seg_binom_tbl(risk_vars = risk_vars_tbl)
        reactable::reactable(
          data = binom_table
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

    shiny::observe({
      output$text_source <- shiny::renderText({
        shiny::validate(
          shiny::need(
            expr = module_data$imported_data(),
            message = "please import data"
          )
        )
        htmltools::HTML(
          paste0(
            shiny::tags$p("The Diabetes Technology Society Blood Glucose Monitor System (BGMS) Surveillance Program confirmed these ranges on 18 blood glucose monitoring systems using pre-determined analytical accuracy criteria agreed upon by the DTS-BGMS Surveillance Committee."),
        shiny::tags$a("Source: J Diabetes Sci Technol 8: 673-684, 2014. PMID: 25562887",
          href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4764239/")
          )
        )
      })
    }) |>
      # bind this to import
      shiny::bindEvent(imported())

  })
}

#' seg_graph UI Function
#'
#' @description A shiny Module.
mod_seg_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::sliderInput(
          width = "220px",
          ns("alpha"),
          label = "Point opacity",
          min = 0.1,
          max = 1,
          step = 0.1,
          value = 0.60
        ),
        shiny::numericInput(
          width = "220px",
          ns("size"),
          label = "Point Size",
          value = 2.4,
          step = 0.1,
          min = 1,
          max = 5
        ),
        shiny::selectInput(
          width = "220px",
          inputId = ns("color"),
          choices = c("#000000", "#222222", "#2F2F2F", "#444444", "#565656"),
          selected = "#000000",
          label = "Point outline color"
        ),
        shiny::selectInput(
          width = "220px",
          inputId = ns("fill"),
          choices = c("#FFFFFF", "#EEEEEE", "#F5F5F5", "#E7F5F7", "#F0F8FF", "#E5E5E5"),
          selected = "#FFFFFF",
          label = "Point fill color"
        )
      ),
    # shiny::fluidRow(
    #   shiny::column(
    #     width = 12,
    # textInput(
    #   inputId = "plot_title",
    #   label = "Plot title",
    #   placeholder = "Enter text to be used as plot title"
    # )))
      shiny::column(
        width = 8, offset = 1,
        shiny::plotOutput(ns("display_grid"), width = "100%")
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' seg_graph Server Functions
#'
#' @description SEG grid server function
#'
mod_seg_graph_server <- function(id, module_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_grid <- shiny::renderPlot({
        seg_grid <- segtools::seg_graph(
          data = imported(),
          alpha_var = input$alpha,
          size_var = input$size,
          color_var = input$color,
          fill_var = input$fill
        )
        seg_grid
      }, res = 108,
        width = 900,
        height = 450
        )
      }) |>
      # bind this to import
      shiny::bindEvent(imported())

    # reactives (dev only)
    # output$values <- shiny::renderPrint({
    #   input$color
      #   shiny::reactiveValuesToList(
      #     x = input,
      #     all.names = TRUE
      #   )
      # remove reactable ids
      # react_ids <- stringr::str_detect(names(vals()),
      #   "__reactable__",
      #   negate = TRUE
      # )
      # vals()[react_ids]
    # })

  })
}

#' modba_graph UI Function
#'
#' @description A shiny Module.
#'
mod_modba_graph_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 10, offset = 1,
        shiny::plotOutput(ns("display_modba"),
          width = "100%")
        # shiny::verbatimTextOutput(ns("values"))
      )
    )
  )
}

#' modba_graph Server Functions
#'
#' @description Mod Bland-Altman graph server function
mod_modba_graph_server <- function(id, module_data){

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # create imported data.frame
    imported <- shiny::reactive({
      shiny::validate(
        shiny::need(
          expr = module_data$imported_data(),
          message = "please import data"
        )
      )
      imported <- module_data$imported_data()
    }) |>
      shiny::bindEvent(module_data$imported_data())

    # use observe for the side-effect here
    shiny::observe({
      output$display_modba <- shiny::renderPlot({
        seg_modba <- segtools::seg_modba_graph(
          data = imported()
        )
        seg_modba
      }, res = 108,
        width = 1000,
        height = 600
        )
    }) |>
      # bind this to import
      shiny::bindEvent(imported())


  })
}
