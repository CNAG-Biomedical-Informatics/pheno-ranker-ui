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

mod_landing_page_ui <- function(id){
  ns <- NS(id)

  grid_container(
    layout = c(
      "         1fr         1fr   ",
      "150px      welcome     welcome ",
      "1fr      utilities   modes   "
    ),
    gap_size = "0px",
    grid_place(
      area = "welcome",
      card_body(
        style = "text-align: center;", 
        h1("Welcome to Pheno-Ranker"),
        p("Your interactive tool for semantic similarity analyses of Phenotypic Data Stored in GA4GH Standards and Beyond")
      )
    ),
    grid_card(
      area = "utilities",
      card_header("Obtain input data"),
      card_body(
        p("
          If you do not have Phenotypic data stored in Beacon-friendly format (BFF) or Phenopackets v2(PXF), 
          you can use the following utilities to get started:"
        ),
        h6("Option 1: Simulate a BFF/PXF using our sample data generator "),
        p("
          Generate a json array of up to 5000 patient with random phenotypic data.
          In order to obtain data that is more similar to your own, you can also provide a list of
          diseases, phenotypic features and treatments that you would like to be included in the simulated data.
        "),
        actionButton(
          "navigateButton", 
          "Simulate BFF/PXF", 
          style = "width: 100%;"
        ),
        hr(),
        h6(
          "Option 2: Convert your own csv data to a json format understandable by Pheno-Ranker"
        ),
        p("
          Upload your own csv file and convert it to a json array that can be used as input for Pheno-Ranker.
          This utility is designed to handle both simple CSV files without nested fields in columns, 
          as well as more complex ones with nested fields 
        "),
        actionButton(
          "navigateButton2", 
          "ConvertCSVs", 
          style = "width: 100%;"
        ),
        hr(),
        h6("Option 3: Convert your phenotypic data to BFF/PXF"),
        p("
          Below you find a software toolkit that allows your own phenotypic data stored in other common formats like RedCap, OMOP, etc. to BFF/PXF.
        "),
        div(
          icon("github"),
          a(
            "Convert-pheno, A software toolkit for the interconversion of standard data models for phenotypic data",
            href = "https://github.com/CNAG-Biomedical-Informatics/convert-pheno",
          )
        )
      )
    ),
    grid_card(
      area = "modes",
      card_header("Modes"),
      card_body(
        p(
          "Pheno-Ranker can be used in two different modes: "
        ),
        h6("Mode 1: Compare your patient to one or multiple reference cohorts"),
        p("
          Rank all the individuals in your 
          reference cohort according to their similarity to your patient.
        "),
        actionButton(
          "navigateButton3", 
          "Individual vs Reference Cohort(s)", 
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
          "navigateButton4", 
          "Intra-/Inter-cohort comparison", 
          style = "width: 100%;"
        )
      )
    )
  )
}

mod_landing_page_server <- function(id, session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}