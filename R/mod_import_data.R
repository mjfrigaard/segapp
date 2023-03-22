#' import_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export mod_import_data_ui
#'
#' @importFrom shiny NS tagList fluidRow
#' @importFrom shiny fileInput column downloadButton
#' @importFrom shiny uiOutput verbatimTextOutput
#' @importFrom htmltools br strong code
#' @importFrom bs4Dash box
mod_import_data_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bs4Dash::box(
      title = "Instructions",
      status = "secondary",
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::tags$p(
            "Upload your data in a comma separated values (",
            shiny::tags$code(".csv"), ") file by clicking on the 'Browse' button below. Your ", shiny::tags$code(".csv"), " file should contain only two columns:"),
          # BGM text ----
          shiny::tags$p("-    Blood glucose monitor (", shiny::tags$strong("BGM"), ") readings should be in the leftmost column under the heading, '", shiny::tags$code("BGM"), "'. These are the meter readings or point-of-care readings."),
          # REF text ----
          shiny::tags$p("-    Reference values (", shiny::tags$strong("REF"), ") should be in the next column under the label, '", shiny::tags$code("REF"), "'. ", shiny::tags$strong("REF"), "values might come from simultaneously obtained plasma specimens run on a laboratory analyzer such as the YSI Life Sciences 2300 Stat Plus Glucose Lactate Analyzer."),
          # concentration text ----
          shiny::tags$p("All glucose concentrations should be in mg/dL and rounded to the nearest integer. If you have any questions about how your ", shiny::tags$code(".csv"), " data file should look before uploading it, please download the sample data set we have provided."),
          # concentration reminder text ----
          shiny::tags$em(
            shiny::tags$strong("Again, all glucose concentrations should be in mg/dL and rounded to the nearest integer.")
          )
        )
      )
    ),
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
#'
#' @param id
#'
#' @return shiny server
#' @export mod_import_data_server
#'
#' @importFrom shiny moduleServer NS renderUI
#' @importFrom shiny renderUI req updateSelectInput
#' @importFrom shiny reactive observe bindEvent
#' @importFrom shiny reactiveValuesToList renderPrint
#' @importFrom htmltools em
#' @importFrom tools file_ext
#' @importFrom stringr str_detect
#' @importFrom readxl excel_sheets
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
          resizable = TRUE,
          highlight = TRUE,
          compact = TRUE,
          wrap = FALSE,
          bordered = TRUE
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

## To be copied in the UI
# mod_import_data_ui("import_data_1")

## To be copied in the server
# mod_import_data_server("import_data_1")
