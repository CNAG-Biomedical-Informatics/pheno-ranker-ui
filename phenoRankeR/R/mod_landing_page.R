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

#* Note: the landing page module is coming without a server function
# because it was not possible to run updateNavbarPage from this module
# the function is called from app_server.R

mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")
  max_individuals <- get_golem_options("maxIndividuals")

  grid_container(
    layout = c(
      "         1fr         1fr     ",
      "1fr      welcome     welcome ",
      "800px    utilities   modes   ",
      "5px      version     version "
    ),
    gap_size = "0px",
    grid_place(
      area = "welcome",
      card_body(
        style = "text-align: center;",
        h3("Welcome to Pheno-Ranker"),
        span(
          "Your interactive tool for semantic similarity analyses
          of Phenotypic Data Stored in GA4GH Standards and Beyond"
        ),
      )
    ),
    grid_card(
      area = "utilities",
      card_header("New User?"),
      mod_decission_tree_ui(
        ns("utilities"),
        ns
      )
    ),
    grid_card(
      area = "modes",
      card_header("Jump right in!"),
      card_body(
        p(
          "Pheno-Ranker can be used in two different modes: "
        ),
        tags$img(
          src = "www/images/patient_mode_representation.svg",
          style = "width: 100%;"
        ),
        h6("Mode 1: Compare your patient to one or multiple reference cohorts"),
        p("
          Rank all the individuals in your
          reference cohort according to their similarity to your patient.
        "),
        actionButton(
          ns("navigateToPatientMode"),
          "Patient vs Reference Cohort(s)",
          style = "width: 100%;"
        ),
        hr(),
        h6("Mode 2: Intra-/Inter-cohort comparison"),
        grid_container(
          layout = c(
            "         1fr         1fr   ",
            "150px    intra       inter "
          ),
          grid_card(
            area = "intra",
            card_header("Intra-cohort comparison"),
            card_body(
              p("
                Rank all the individuals in your
                reference cohort according to their similarity to each other.
              ")
            )
          ),
          grid_card(
            area = "inter",
            card_header("Inter-cohort comparison"),
            card_body(
              p("
                Compare two or more cohorts with each other.
              ")
            )
          )
        ),
        actionButton(
          ns("navigateToCohortMode"),
          "Intra-/Inter-cohort comparison",
          style = "width: 100%;"
        )
      )
    ),
    grid_place(
      area = "version",
      card_body(
        style = "text-align: right;",
        p("Version ", version)
      )
    )
  )
}
