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

# Utility function to create a card with an action button and image
create_mode_card <- function(ns, area, header, button_id, button_text, img_src) {
  
  img_style <- "height: 200px; margin-top: -12px;"
  if (area == "patient_mode") {
    img_style <- "height: 150px; margin-top: 37px;"
  }
  
  grid_card(
    area = area,
    card_header(header),
    card_body(
      padding = "5px",
      actionButton(
        ns(button_id),
        button_text,
        style = "background-color: #1976d2; color: white;"
      ),
      img(
        src = img_src,
        style = img_style
      )
    )
  )
}

old_layout <- c(
  "         650px       1fr     ",
  "1fr      welcome     welcome ",
  "800px    new_user    modes   ",
  "5px      version     version "
)

mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")
  max_individuals <- get_golem_options("maxIndividuals")

  grid_container(
    layout = c(
      "         1fr           1fr     ",
      "1fr      welcome       welcome ",
      "300px    cohort_mode   patient_mode",
      "600px    new_user      new_user",
      "5px      version       version "
    ),
    gap_size = "0px",
    grid_place(
      area = "welcome",
      div(
        h3("Welcome to Pheno-Ranker"),
        span(
          "Your interactive tool for semantic similarity analyses
          of Phenotypic Data Stored in GA4GH Standards and Beyond"
        ),
        style = "margin-bottom: 0px; text-align: center;"
      )
    ),
    create_mode_card(
      ns,
      "cohort_mode",
      "Intra-/Inter-cohort comparison",
      "navigateToCohortMode2",
      "Go to Cohort mode",
      "www/images/cohort_mode_icon.svg"
    ),
    create_mode_card(
      ns,
      "patient_mode",
      "Patient vs Reference Cohort(s)",
      "navigateToPatientMode2",
      "Go to Patient mode",
      "www/images/patient_mode_icon.svg"
    ),
    grid_card(
      area = "new_user",
      card_header("New User?"),
      mod_decision_tree_ui(
        ns("utilities"),
        ns
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
