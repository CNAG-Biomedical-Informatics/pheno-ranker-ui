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
            max-width: 20px;
          }
          .pxf {
            max-width: 20px;
          }
          .hover-info {
            display: none;
            position: absolute;
            background-color: #f9f9f9;
            padding: 10px;
            box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.2);
            z-index: 100;
            width: 300px;
            top: 100%; /* Position it below the node */
            left: 50%;
            transform: translateX(-50%); /* Center it */
            text-align: left;
          }

          .hover-info .process {
            all: unset;
          }

          .hover-info ul::before,
          .hover-info ul::after,
          .hover-info li::before,
          .hover-info li::after {
            display: none;
          }

          /* Additional reset for any ul  */
          .hover-info ul {
            padding: 0;
            margin: 0;
          }

          .hover-info li {
            padding: 5px;
            margin: 0;
          }

          .hover-info li a {
            padding: 0;
            margin: 0;
          }
          /* Only show the hover-info div when hovering over the parent li element */
          .tree li:hover > .hover-info {
            display: block;
          }
        ")),
        ),
        p(
          "Our interactive decision tree will help you
          to get started with Pheno-Ranker 🚀"
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
                    "Simulate BFF/PXF",
                    div(
                      class = "hover-info",
                      "Generate a json array of up to 2500 patients
                      with random phenotypic data. In order to obtain data
                      that is more similar to your own,
                      you can also provide a list of diseases, phenotypic features
                      and treatments that you would like to be included
                      in the simulated data.",
                      tags$a(href = "#", "learn more")
                    )
                  )),
                  tags$li(tags$a(href = "#", class = "process",
                    div(class = "icon", tags$i(class = "fas fa-hand-pointer")),
                    "Get example data",
                    div(
                      class = "hover-info",
                      "Get example data from public sources like the
                      Phenopacket store",
                      tags$a(href = "#", "learn more")
                    )
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
                          tags$img(src = "https://avatars.githubusercontent.com/u/17553567?s=200&v=4", class = "pxf")
                        )
                      ),
                      "BFF/PXF",
                      div(
                        class = "hover-info",
                        p("Formats supported out of the box"),
                        tags$ul(
                          tags$li(
                            tags$a(
                              href = "https://phenopacket-schema.readthedocs.io/en/1.0.0/basics.html",
                              "Beacon-friendly format (BFF)"
                            ),
                            tags$a(
                              href = "https://phenopacket-schema.readthedocs.io/en/1.0.0/basics.html",
                              "Phenopacket exchange format v2 (PXF)"
                            ),
                          )
                        ),
                        p("Both are data exchange formats established by"),
                        tags$a(
                          href = "https://beacon-project.io/",
                          tags$img(
                            src = "https://www.ga4gh.org/wp-content/themes/ga4gh/dist/assets/svg/logos/logo-full-color.svg",
                          )
                        ),
                        tags$a(href = "#", "learn more"),
                        p("or jump right in!"),
                        # two buttons one for patient mode and one for cohort mode
                        actionButton(
                          ns("navigateToPatientMode"),
                          "Patient vs Reference Cohort(s)",
                          style = "width: 100%;"
                        ),
                        actionButton(
                          ns("navigateToCohortMode"),
                          "Intra-/Inter-cohort comparison",
                          style = "width: 100%;"
                        )
                      )
                    )
                  ),
                  tags$li(
                    tags$a(href = "#", class = "process",
                      div(class = "icon", tags$i(class = "fas fa-file-csv")),
                      "generic CSV",
                      div(
                        class = "hover-info",
                        "Upload your own csv file and convert it
                        to a json array that can be used as input for Pheno-Ranker.
                        This utility is designed to handle both
                        simple CSV files without nested fields in columns,
                        as well as more complex ones with nested fields",
                        tags$a(href = "#", "learn more"),
                        p("or try it right away!"),
                        actionButton(
                          ns("navigateToConverter"),
                          "ConvertCSVs",
                          style = "width: 100%;"
                        )
                      )
                    )
                  ),
                  tags$li(
                    tags$a(href = "#", class = "process",
                      div(class = "icon", tags$i(class = "fas fa-file")),
                      HTML("RedCap,<br/>OMOP-CDM,<br/>CDISC-ODM<br/>"),
                      div(
                        class = "hover-info",
                        "Below you find a software toolkit that allows
                        your own phenotypic data stored in other
                        common formats like RedCap, OMOP, etc. to BFF/PXF.",
                        tags$a(
                          href = "https://github.com/CNAG-Biomedical-Informatics/convert-pheno",
                          target = "_blank",
                          img(
                            src="https://raw.githubusercontent.com/cnag-biomedical-informatics/convert-pheno/main/docs/img/CP-logo.png",
                            style = "width: 50px;"
                          ),
                          img(
                            src="https://raw.githubusercontent.com/cnag-biomedical-informatics/convert-pheno/main/docs/img/CP-text.png",
                            style = "width: 200px;"
                          )
                        ),
                      )
                    )
                  )
                )
              )
            )
          )
        )),
        p(
          "Hover over the bottom nodes to learn more💡"
        ),
        p(
          "Hint: they are clickable!👇"
        ),
      )
    ),
    grid_card(
      area = "modes",
      card_header("Jump right in!"),
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
