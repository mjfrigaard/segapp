#' Run dev version of app
source("utils.R")
source("modules.R")

library(shiny)
library(ggplot2)
library(bs4Dash)
library(reactable)
# install.packages('remotes')
# ensure current version
# remotes::install_github("mjfrigaard/segtools", force = TRUE, quiet = TRUE)
# remotes::install_github("mjfrigaard/segapp", force = TRUE, quiet = TRUE)
library(segtools)
library(segapp)

dev_ui <- function() {
  shiny::tagList(
    bs4Dash::dashboardPage(
      dark = FALSE,
      title = "",
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "(dev) Surveillance error grid",
          color = "secondary"
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        minified = TRUE,
        collapsed = FALSE,
        bs4Dash::sidebarMenu(
          id = "tabs",
          bs4Dash::menuItem("Start", tabName = "tab_import"),
          bs4Dash::menuItem("MARD", tabName = "tab_mard"),
          bs4Dash::menuItem("Risk Tables", tabName = "tab_risk_tbls"),
          bs4Dash::menuItem("Compliant Pairs", tabName = "tab_iso"),
          bs4Dash::menuItem("Surveillance Error Grid", tabName = "tab_seg"),
          bs4Dash::menuItem("Modified Bland-Altman Plot", tabName = "tab_mbap")
        )
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "tab_import",
            # module 'mod_import_data' ----
            shiny::fluidRow(
              bs4Dash::sortable(
                bs4Dash::box(
                  title = "Instructions",
                  status = "secondary",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  shiny::fluidRow(
                    shiny::column(
                      width = 12,
                      shiny::tags$p(
                        "Upload your data in a comma separated values (",
                        shiny::tags$code(".csv"),
                        ") file by clicking on the 'Browse' button below. Your ",
                        shiny::tags$code(".csv"), " file should contain only two columns:"
                      ),
                      # BGM text ----
                      shiny::tags$p("-    Blood glucose monitor (",
                        shiny::tags$strong("BGM"), ") readings should be in the leftmost column under the heading, '",
                        shiny::tags$code("BGM"), "'. These are the meter readings or point-of-care readings."),
                      # REF text ----
                      shiny::tags$p("-    Reference values (",
                        shiny::tags$strong("REF"), ") should be in the next column under the label, '",
                        shiny::tags$code("REF"), "'. ",
                        shiny::tags$strong("REF"), "values might come from simultaneously obtained plasma specimens run on a laboratory analyzer such as the YSI Life Sciences 2300 Stat Plus Glucose Lactate Analyzer."),
                      # concentration text ----
                      shiny::tags$p("All glucose concentrations should be in mg/dL and rounded to the nearest integer. If you have any questions about how your ",
                        shiny::tags$code(".csv"), " data file should look before uploading it, please download the sample data set we have provided."),
                      # concentration reminder text ----
                      shiny::tags$em(
                        shiny::tags$strong("Again, all glucose concentrations should be in mg/dL and rounded to the nearest integer.")
                      )
                    )
                  )
                ),
                # module 'mod_import_data_ui' ----
                mod_import_data_ui("data")
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "tab_mard",
            shiny::fluidRow(
              bs4Dash::sortable(
                bs4Dash::box(
                  title = "Pair Types", width = 12,
                  # module 'mod_pair_type' ----
                  mod_pair_type_ui("pairs")
                )
              )
            ),
            shiny::fluidRow(
              bs4Dash::sortable(
                bs4Dash::box(
                  title = "Mean absolute relative difference (MARD)",
                  width = 12,
                  collapsed = FALSE,
                  # module 'mod_mard_table' ----
                  mod_mard_table_ui("mard")
                )
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "tab_risk_tbls",
            shiny::fluidRow(
              bs4Dash::sortable(
                bs4Dash::box(
                  title = "Risk Grades", width = 12,
                  collapsed = FALSE,
                  # module 'mod_risk_grade' ----
                  mod_risk_grade_ui("grade")
                )
              )
            ),
            shiny::fluidRow(
              bs4Dash::sortable(
                bs4Dash::box(
                  title = "Risk Categories",
                  width = 12,
                  collapsed = FALSE,
                  # module 'mod_risk_category' ----
                  mod_risk_category_ui("cat")
                )
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "tab_iso",
            shiny::fluidRow(
              bs4Dash::sortable(
                bs4Dash::box(
                  title = "Compliant Pairs",
                  width = 12,
                  collapsed = FALSE,
                  # module 'mod_iso_range' ----
                  mod_iso_range_ui("iso"),
                  # module 'mod_compliant_pairs' ----
                  mod_compliant_pairs_ui("comp_pairs")
                )
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "tab_seg",
            shiny::fluidRow(
              bs4Dash::sortable(
                # module 'mod_seg_graph' ----
                bs4Dash::box(
                  title = "Surveillance Error Grid",
                  width = 12,
                  height = "600px",
                  mod_seg_graph_ui("graph")
                )
              )
            )
          ),
          bs4Dash::tabItem(
            tabName = "tab_mbap",
            shiny::fluidRow(
              bs4Dash::sortable(
                # module 'mod_modba_graph' ----
                bs4Dash::box(
                  title = "Modified Bland-Altman Plot",
                  width = 12,
                  height = "700px",
                  mod_modba_graph_ui("modba")
                )
              )
            )
          )
        )
      ),
      controlbar = bs4Dash::dashboardControlbar()
    )
  )
}

dev_server <- function(input, output, session) {

  import <- mod_import_data_server("data")

  mod_pair_type_server("pairs", module_data = import)

  mod_mard_table_server("mard", module_data = import)

  mod_risk_grade_server("grade", module_data = import)

  mod_risk_category_server("cat", module_data = import)

  mod_iso_range_server("iso", module_data = import)

  mod_compliant_pairs_server("comp_pairs", module_data = import)

  mod_seg_graph_server("graph", module_data = import)

  mod_modba_graph_server("modba", module_data = import)

}

run_dev <- function() {
  shiny::shinyApp(ui = dev_ui, server = dev_server)
}
run_dev()
