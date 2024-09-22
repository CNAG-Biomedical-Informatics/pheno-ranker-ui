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
# the function is called from the app_server.R file


mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")
  max_individuals <- get_golem_options("maxIndividuals")

  grid_container(
    layout = c(
      "         1fr         1fr     ",
      "150px    welcome     welcome ",
      "1fr      utilities   modes   ",
      "5px      version     version "
    ),
    gap_size = "0px",
    grid_place(
      area = "welcome",
      card_body(
        style = "text-align: center;",
        h1("Welcome to Pheno-Ranker"),
        p(
          "Your interactive tool for semantic similarity analyses
          of Phenotypic Data Stored in GA4GH Standards and Beyond"
        )
      )
    ),
    grid_card(
      area = "utilities",
      card_header("Obtain input data"),
      card_body(
        tags$head(
          tags$style(HTML("
          .tree ul {
            padding-top: 20px;
            position: relative;
            transition: all 0.5s;
            display: flex;
            justify-content: center;
            padding-left: 0;
          }
          .tree li {
            display: inline-block;
            text-align: center;
            list-style-type: none;
            position: relative;
            padding: 20px 5px 0 5px;
            transition: all 0.5s;
          }
          .tree li::before,
          .tree li::after {
            content: '';
            position: absolute;
            top: 0;
            right: 50%;
            border-top: 1px solid #ccc;
            width: 50%;
            height: 20px;
          }
          .tree li::after {
            right: auto;
            left: 50%;
            border-left: 1px solid #ccc;
          }
          .tree li:only-child::after,
          .tree li:only-child::before {
            display: none;
          }
          .tree li:only-child {
            padding-top: 0;
          }
          .tree li:first-child::before,
          .tree li:last-child::after {
            border: 0 none;
          }
          .tree li:last-child::before {
            border-right: 1px solid #ccc;
            border-radius: 0 5px 0 0;
          }
          .tree li:first-child::after {
            border-radius: 5px 0 0 0;
          }
          .tree ul ul::before {
            content: '';
            position: absolute;
            top: 0;
            border-left: 1px solid #ccc;
            width: 0;
            height: 20px;
          }
          .tree li a {
            border: 1px solid #ccc;
            padding: 10px 15px;
            text-decoration: none;
            color: #666;
            font-size: 14px;
            display: inline-block;
            border-radius: 5px;
            transition: all 0.3s ease;
            background-color: #e3f2fd;
            color: #1565c0;
            position: relative;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
          }
          .tree li a:hover {
            background-color: #c8e4f8;
            color: #000;
            border: 1px solid #94a0b4;
          }
          .icon {
            font-size: 20px;
            margin-right: 5px;
          }
          .decision {
            background-color: #fff9c4;
            color: #f57f17;
            padding: 10px 15px;
          }
          .process {
            background-color: #e3f2fd;
            color: #1565c0;
            padding: 10px 15px;
          }
          .label {
            position: absolute;
            background-color: #fff;
            padding: 2px 5px;
            font-size: 12px;
            color: #333;
            border: 1px solid #ccc;
            border-radius: 3px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
            z-index: 999;
          }
          .label-no {
            right: 20%; 
            top: -5%;
          }
          .label-yes {
            right: 70%; 
            top: -5%;
          }
          .row {
            display: flex;
            align-items: center;
            justify-content: flex-start;
          }
          .column {
            flex: 0 0 auto;
          }
          .bff {
            max-width: 48px;
          }
          .pxf {
            max-width: 70px;
          }
        ")),
        ),
        div(class = "tree",
        tags$ul(
          tags$li(
            tags$a(href = "#", class = "decision",
              div(class = "icon", tags$i(class = "fas fa-question-circle")),
              "Do you have data?"
            ),
            tags$ul(
              tags$li(
                div(class = "label label-no", "No"),
                tags$a(href = "#", class = "decision",
                  div(class = "icon", tags$i(class = "fas fa-wand-magic-sparkles")),
                  "Use our utilities to get data"
                ),
                tags$ul(
                  tags$li(tags$a(href = "#", class = "process",
                    div(class = "icon", tags$i(class = "fas fa-flask")),
                    "Simulate BFF/PXF"
                  )),
                  tags$li(tags$a(href = "#", class = "process",
                    div(class = "icon", tags$i(class = "fas fa-hand-pointer")),
                    "Get example data"
                  ))
                )
              ),
              tags$li(
                div(class = "label label-yes", "Yes"),
                tags$a(href = "#", class = "decision",
                  div(class = "icon", tags$i(class = "fas fa-file-alt")),
                  "Which file type?"
                ),
                tags$ul(
                  tags$li(
                    tags$a(href = "#", class = "process",
                      div(class = "row",
                        div(class = "column",
                          tags$img(src = "https://avatars.githubusercontent.com/u/33450937?s=48&v=4", class = "bff")
                        ),
                        div(class = "column",
                          tags$img(src = "https://pubcasefinder.dbcls.jp/static/images/pcf/top/logo_phenopackets.png", class = "pxf")
                        )
                      ),
                      "BFF/PXF"
                    )
                  ),
                  tags$li(
                    tags$a(href = "#", class = "process",
                      div(class = "icon", tags$i(class = "fas fa-file-csv")),
                      "generic CSV"
                    )
                  ),
                  tags$li(
                    tags$a(href = "#", class = "process",
                      div(class = "icon", tags$i(class = "fas fa-file")),
                      HTML("RedCap,<br/>OMOP-CDM,<br/>CDISC-ODM<br/>")
                    )
                  )
                )
              )
            )
          )
        )),
        # div(
        #   class = "flowchart-container",
        #   # Start node
        #   div(
        #     class = "circle",
        #     style = "top: 20px; left: 20px;", "Start"
        #   ),

        #   # Line from Start to A
        #   div(
        #     class = "line vertical",
        #     style = "top: 120px; left: 20px; height: 80px;"
        #   ),
        #   div(
        #     class = "arrow down",
        #     style = "top: 200px; left: 20px;"
        #   ),

        #   # Node A
        #   div(
        #     class = "diamond", style = "top: 200px; left: 350px;",
        #     div(class = "diamond-text", "Is input data available?")
        #   ),

        #   # Line from A to B ('No' path)
        #   div(class = "line vertical", style = "top: 300px; left: 400px; height: 80px;"),
        #   div(class = "arrow down", style = "top: 380px; left: 390px;"),
        #   div(class = "label", style = "top: 330px; left: 410px;", "No"),

        #   # Node B
        #   div(
        #     class = "diamond", style = "top: 380px; left: 350px;",
        #     div(class = "diamond-text", "Which data source?")
        #   ),

        #   # Line from A to G ('Yes' path)
        #   div(class = "line horizontal", style = "top: 250px; left: 400px; width: 150px;"),
        #   div(class = "arrow right", style = "top: 240px; left: 550px;"),
        #   div(class = "label", style = "top: 230px; left: 475px;", "Yes"),

        #   # Node G
        #   div(
        #     class = "diamond", style = "top: 200px; left: 600px;",
        #     div(class = "diamond-text", "Which file type?")
        #   ),

        #   # Line from B to D (Left)
        #   div(class = "line horizontal", style = "top: 430px; left: 350px; width: -150px; transform: scaleX(-1);"),
        #   div(class = "arrow left", style = "top: 420px; left: 200px;"),

        #   # Node D
        #   div(class = "rectangle", style = "top: 400px; left: 50px;", "Example data"),

        #   # Line from B to C (Right)
        #   div(class = "line horizontal", style = "top: 430px; left: 400px; width: 200px;"),
        #   div(class = "arrow right", style = "top: 420px; left: 600px;"),

        #   # Node C
        #   div(class = "rectangle", style = "top: 400px; left: 600px;", "Simulated BFF/PXF"),

        #   # Line from G to H (Up)
        #   div(class = "line vertical", style = "top: 150px; left: 650px; height: -80px;"),
        #   div(class = "arrow up", style = "top: 70px; left: 640px;"),

        #   # Node H
        #   div(class = "rectangle", style = "top: 20px; left: 600px;", "CSV"),

        #   # Line from G to F (Down)
        #   div(class = "line vertical", style = "top: 300px; left: 650px; height: 100px;"),
        #   div(class = "arrow down", style = "top: 400px; left: 640px;"),

        #   # Node F
        #   div(class = "rectangle", style = "top: 400px; left: 600px;", "BFF/PXF")
        # ),
        p("
          If you do not have Phenotypic data stored in
          Beacon-friendly format (BFF) or Phenopackets v2(PXF),
          you can use the following utilities to get started:"),
        h6("Option 1: Simulate a BFF/PXF using our sample data generator "),
        p(paste(
          "Generate a json array of up to", max_individuals,
          "patient with random phenotypic data. In order to obtain data
          that is more similar to your own,
          you can also provide a list of diseases, phenotypic features
          and treatments that you would like to be included
          in the simulated data."
        )),
        actionButton(
          ns("navigateToSimulator"),
          "Simulate BFF/PXF",
          style = "width: 100%;"
        ),
        hr(),
        h6("Option 2: Get example data from public sources"),
        p("
          Get example data from public sources like the
        "),
        a("
          Phenopacket store",
          href = "https://monarch-initiative.github.io/phenopacket-store",
          target = "_blank"
        ),
        actionButton(
          ns("navigateToExamples"),
          "Get example data",
          style = "width: 100%;"
        ),
        hr(),
        h6(
          "Option 3: Convert your own csv data
          to a json format understandable by Pheno-Ranker"
        ),
        p("
          Upload your own csv file and convert it
          to a json array that can be used as input for Pheno-Ranker.
          This utility is designed to handle both
          simple CSV files without nested fields in columns,
          as well as more complex ones with nested fields
        "),
        actionButton(
          ns("navigateToConverter"),
          "ConvertCSVs",
          style = "width: 100%;"
        ),
        hr(),
        h6("Option 4: Convert your phenotypic data to BFF/PXF"),
        p("
          Below you find a software toolkit that allows
          your own phenotypic data stored in other
          common formats like RedCap, OMOP, etc. to BFF/PXF.
        "),
        div(
          icon("github"),
          a(
            "Convert-Pheno: A software toolkit for the
            interconversion of standard data models for phenotypic data",
            href = "https://github.com/CNAG-Biomedical-Informatics/convert-pheno",
            target = "_blank"
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
