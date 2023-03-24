#' development application server-side
#'
#' @param input,output,session Internal parameters for {shiny}
#'
#' @export dev_server
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
