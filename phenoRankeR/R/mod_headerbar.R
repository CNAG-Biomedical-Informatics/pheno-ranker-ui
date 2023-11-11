#' Landing Page Module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom gridlayout grid_container grid_card grid_place
#' @importFrom shiny NS


mod_headerbar_ui <- function(id){
  ns <- NS(id)
  div( class = "side-by-side-container",
    # actionButton(
    #   "btn_toggle_dark_mode",
    #   label = "",
    #   icon = icon("moon"),
    #   style = "height: 60px; width: 60px;"
    # ),

    # TODO
    # depending on the mode (dark vs white) the color should be different
    a(
      id = "link_docs",
      href = "https://cnag-biomedical-informatics.github.io/pheno-ranker",
      target = "_blank",
      icon("book-open-reader"),
      style = "font-size: 2.5em; color: black;"
    ),
    a(
      id = "link_github",
      href = "https://github.com/CNAG-Biomedical-Informatics/pheno-ranker",
      target = "_blank",
      icon("github"),
      style = "font-size: 2.5em; color: black;"
    )
  )
}

mod_headerbar_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}