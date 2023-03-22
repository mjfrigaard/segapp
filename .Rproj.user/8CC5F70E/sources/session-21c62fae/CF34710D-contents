#' development application UI
#'
#'
#' @export dev_ui
dev_ui <- function() {
  shiny::tagList(
    bs4Dash::dashboardPage(
      title = "",
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "SURVEILLANCE ERROR GRID",
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
                    collapsed = FALSE
                    # module 'mod_mard_table' ----
                    # mod_mard_table_ui("mard")
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
                    collapsed = FALSE
                    # module 'mod_risk_grade' ----
                    # mod_risk_grade_ui("grade")
                  )
                )
              ),
              shiny::fluidRow(
                bs4Dash::sortable(
                  bs4Dash::box(
                    title = "Risk Categories",
                    width = 12,
                    collapsed = FALSE
                    # module 'mod_risk_level' ----
                    # mod_risk_level_ui("level")
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
                    collapsed = FALSE
                    # module 'mod_iso_range' ----
                    # mod_iso_range_ui("iso")
                  )
                  # mod_compliant_pairs_ui("comp_pairs")
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
                    width = 12
                    # mod_seg_graph_ui("graph")
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
                    width = 12
                    # mod_modba_graph_ui("modba")
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
