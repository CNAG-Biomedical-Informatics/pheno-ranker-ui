#' Decission tree Module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom gridlayout grid_container grid_card grid_place new_gridlayout
#' @importFrom bslib card_header card_body
#' @importFrom shiny NS actionButton icon


render_decision_tree <- function(ns) {
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
        max-width: 700px;
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
        padding: 10px 0px;
        text-decoration: none;
        color: #666;
        font-size: 14x;
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
        padding: 10px 15px !important;
      }
      .process {
        background-color: #e3f2fd;
        color: #1565c0;
        min-height: 124px;
        min-width: 130px;
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
        z-index: 99;
      }
      .label-no {
        right: 5%;
        top: -5%;
      }
      .label-yes {
        right: 65%;
        top: -5%;
      }
      .bff {
        max-height: 50px;
        padding-left: 30px;
        margin-bottom: 5px;
      }
      .pxf {
        max-width: 50px;
      }
      .hover-info {
        display: none;
        position: absolute;
        background-color: #f9f9f9;
        padding: 10px;
        box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.2);
        z-index: 100;
        width: 300px;
        top: -120%; /* Position it on the height of the node */
        left: 100%; /* Position it to the right of the node */
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

      .hover-info .link {
        display: inline;
        padding: 0 ;
        margin: 0 ;
        border: none;
        background: none;
        box-shadow: none;
        font-weight: normal;
        text-decoration: underline;
        color: #1565c0;
      }
    "))
    ),
    div(
      class = "tree",
      tags$ul(
        style = "padding-top: 0;",
        tags$li(
          tags$a(
            href = "#", class = "decision",
            div(
              class = "icon",
              tags$i(class = "fas fa-question-circle")
            ),
            "Do you have data?"
          ),
          tags$ul(
            tags$li(
              div(class = "label label-no", "No"),
              tags$a(
                href = "#", class = "decision",
                div(class = "icon", tags$i(class = "fas fa-wand-magic-sparkles")),
                "Use our utilities"
              ),
              tags$ul(
                tags$li(tags$a(
                  style = "padding: 0;",
                  actionButton(
                    ns("navigateToSimulator"),
                    class = "process",
                    div(
                      icon(
                        "flask",
                        class = "icon"
                      ),
                      HTML("<br/> Simulate <br/>BFF/<br/>PXF")
                    )
                  ),
                  div(
                    class = "hover-info",
                    "Generate a json array of up to 2500 patients
                  with random phenotypic data. In order to obtain data
                  that is more similar to your own,
                  you can also provide a list of diseases, phenotypic features
                  and treatments that you would like to be included
                  in the simulated data.",
                    tags$a(href = "#", "learn more"),
                    "or jump right in!",
                    actionButton(
                      ns("navigateToSimulator2"),
                      "Simulate",
                      style = "width: 100%;"
                    )
                  ),
                )),
                tags$li(tags$a(
                  href = "#", class = "process",
                  div(
                    class = "icon",
                    tags$i(class = "fas fa-hand-pointer")
                  ),
                  HTML(
                    "Get <br/>example <br/>data"
                  ),
                  div(
                    class = "hover-info",
                    "Get example data from public sources like the
                  Phenopacket store or public Beacon APIs.",
                    tags$a(
                      href = "#",
                      "learn more"
                    ),
                    "or jump right in!",
                    actionButton(
                      ns("navigateToExamples"),
                      "Get example data",
                      style = "width: 100%;"
                    ),
                    actionButton(
                      ns("navigateToBeaconAPIs"),
                      "Query Beacon API",
                      style = "width: 100%;"
                    )
                  )
                ))
              )
            ),
            tags$li(
              div(class = "label label-yes", "Yes"),
              tags$a(
                href = "#", class = "decision",
                div(
                  class = "icon",
                  tags$i(class = "fas fa-file-alt")
                ),
                "Which file type?"
              ),
              tags$ul(
                tags$li(
                  tags$a(
                    href = "#",
                    class = "process",
                    grid_container(
                      layout = c(
                        "     1fr 1fr  ",
                        "1fr  bff  pxf  "
                      ),
                      gap_size = "0px",
                      grid_place(
                        area = "bff",
                        img(
                          src = "www/img/beacon_logo.png",
                          class = "bff"
                        )
                      ),
                      grid_place(
                        area = "pxf",
                        img(
                          src = "www/img/PhenoPackets_Logo.png",
                          class = "pxf"
                        )
                      )
                    ),
                    HTML("BFF/PXF</br>(JSON)"),
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
                          )
                        )
                      ),
                      span(
                        "Both are data exchange formats established by Global Alliance for Genomics and Health",
                        tags$a(
                          href = "https://ga4gh.org/",
                          class = "link",
                          "(GA4GH)",
                        )
                      ),
                      tags$a(
                        href = "#",
                        "learn more"
                      ),
                      p("or jump right in!"),
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
                  tags$a(
                    href = "#",
                    class = "process",
                    div(
                      class = "icon",
                      tags$i(class = "fas fa-file-csv")
                    ),
                    HTML("generic</br>tabular data<br/>(CSV)"),
                    div(
                      class = "hover-info",
                      "Upload your own csv file and convert it
                    to a json array that can be used as input for Pheno-Ranker.
                    This utility is designed to handle both
                    simple CSV files without nested fields in columns,
                    as well as more complex ones with nested fields",
                      tags$a(
                        href = "#",
                        "learn more"
                      ),
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
                  tags$a(
                    href = "#",
                    class = "process",
                    div(
                      class = "icon",
                      tags$i(class = "fas fa-file")
                    ),
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
                          src = "https://raw.githubusercontent.com/cnag-biomedical-informatics/convert-pheno/main/docs/img/CP-logo.png",
                          style = "width: 50px;"
                        ),
                        img(
                          src = "https://raw.githubusercontent.com/cnag-biomedical-informatics/convert-pheno/main/docs/img/CP-text.png",
                          style = "width: 200px;"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

landing_page_layout <- new_gridlayout(
  c("explanation tree"),
  col_sizes = c("610px", "1fr"),
  alternate_layouts = list(
    # layout = c(
    #   "       250px            1fr     ",
    #   "1fr    explanation      tree    "
    # ),
    layout = c(
      "         1fr         ",
      "100px    explanation ",
      "1fr      tree        "
    ),
    width_bounds = c(max = 1100)
  )
)

mod_decision_tree_ui <- function(id, ns) {
  card_body(
    grid_container(
      layout = landing_page_layout,
      gap_size = "0px",
      grid_place(
        area = "explanation",
        card_body(
          span(
            "Our interactive decision tree will help you
            to get started with Pheno-Ranker \U1F680", # rocket emoji
          ),
          span(
            "Hover over the bottom nodes to learn more \U1F4A1" # light bulb emoji
          ),
          span(
            "Hint: certain nodes are clickable! \U1F447" # pointing finger emoji
          ),
          style = "min-height: 425px;"
        )
      ),
      grid_place(
        area = "tree",
        render_decision_tree(ns)
      )
    )
  )
}
