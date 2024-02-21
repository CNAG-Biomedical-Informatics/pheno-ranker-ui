#' Landing Page Module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom gridlayout grid_container grid_card grid_place
#' @importFrom bslib card_header card_body
#' @importFrom shiny NS actionButton

mod_navbar_ui <- function(id){
	ns <- NS(id)
  
  fluidRow(
    column(
      width = 12,
      title = "placeholder",
      icon(
        name = NULL,
        class = "logo-icon"
      ),
      tabPanel(
        title = "Home",
        mod_landing_page_ui("landing_page")
      ),
      navbarMenu(
        "Rank",
        tabPanel(
          title = "Individual Comparisons",
          value = "patient",
          mod_history_sidebar_ui(
            "PatientHistorySidebar"
          ),
          mod_patient_mode_ui(
            "patient_mode"
          )
        ),
        tabPanel(
          title = "Cohort Comparisons",
          value = "cohort",
          mod_history_sidebar_ui(
            "CohortHistorySidebar"
          ),
          mod_cohort_mode_ui(
            "cohort_mode"
          )
        )
      ),
      navbarMenu(
        "Utilities",
        tabPanel(
          title = "Simulate BFF/PXF",
          value = "sim",
          mod_history_sidebar_ui(
            "SimulateHistorySidebar"
          ),
          mod_sim_mode_ui("sim_mode")
        ),
        tabPanel(
          title = "CSV Conversion",
          value = "conv",
          mod_history_sidebar_ui(
            "ConvertHistorySidebar"
          ),
          mod_conv_mode_ui("conv_mode")
        )
      ),
      tabPanel(
        title = "About",
        mod_about_page_ui("about_page")
      )
    )
  )
}