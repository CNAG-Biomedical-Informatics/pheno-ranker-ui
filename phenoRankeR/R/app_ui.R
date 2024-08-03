#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom bslib bs_theme
#' @importFrom shinyjs useShinyjs
#' @import yaml
#' @noRd

# TODO
# suggestion by Manuel create for each run output a folder with
# all the output files and the used settings

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    navbarPage(
      useShinyjs(),
      id = "nav",
      collapsible = TRUE,
      # tags$script("
      #   Shiny.addCustomMessageHandler('changeURL', function(message) {
      #     var url = new URL (window.location.origin + window.location.pathname)
      #     url.searchParams.set('mode', message.mode);
      #     url.searchParams.set('id', message.id);
      #     history.pushState(null, '', url);
      #   });
      # "),
      selected = "Home",
      theme = bs_theme(version = 5, bootswatch = "default"),
      # theme = bs_theme(version = 5, bootswatch = "darkly"),

      # the title works as placeholder otherwise the logo 
      # will overlay the navbar elements
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
          title = "Patient Mode",
          value = "patient",
          mod_history_sidebar_ui(
            "PatientHistorySidebar"
          ),
          mod_patient_mode_ui(
            "patient_mode"
          )
        ),
        tabPanel(
          title = "Cohort Mode",
          value = "cohort",
          mod_history_sidebar_ui(
            "CohortHistorySidebar"
          ),
          mod_cohort_mode_ui(
            "cohort_mode"
          )
        ),
      ),
      navbarMenu(
        "Utilities",
        tabPanel(
          title = "Get example input",
          value = "input_examples",
          mod_input_examples_page_ui("input_examples")
        ),
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
      ),
      mod_headerbar_ui("headerbar")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "phenoRankeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
