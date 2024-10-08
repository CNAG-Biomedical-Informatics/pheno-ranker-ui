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

mod_about_page_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")

  grid_container(
    layout = c(
      "       1fr",
      "200px  content"
    ),
    grid_place(
      area = "content",
      card_body(
        style = "text-align: center;",
        h3("Pheno-Ranker"),
        p("Advancing Semantic Similarity Analysis of Phenotypic Data Stored in GA4GH Standards and Beyond"),
        h4("Please cite:"),
        p(
          "Leist, I.C. et al.,",
          "Pheno-Ranker: A Toolkit for Comparison of Phenotypic Data Stored in GA4GH Standards and Beyond",
          ",Submitted."
        ),
        h4("Developed by:"),
        grid_container(
          layout = c(
            "       1fr       1fr       1fr     ",
            "100px  Manuel    Ivo       Sofia   ",
            "100px  divider   divider   divider ",
            "200px  logo1     logo2     logo3   ",
            "1px   version   version   version  "
          ),
          grid_card(
            area = "Manuel",
            card_header("Manuel Rueda, PhD"),
            card_body(
              style = "text-align: center;",
              p("Pheno-Ranker Perl module")
            )
          ),
          grid_card(
            area = "Ivo",
            card_header("Ivo C. Leist"),
            card_body(
              style = "text-align: center;",
              p("Pheno-Ranker UI (this R-Shiny app)")
            )
          ),
          grid_card(
            area = "Sofia",
            card_header("Sofia Chaves"),
            card_body(
              style = "text-align: center;",
              p("Design and UX")
            )
          ),
          grid_place(
            area = "divider",
            card_body(
              br(),
              hr(),
              h3("Acknowledgements:"),
            )
          ),
          grid_place(
            area = "logo1",
            card_body(
              style = "text-align: center;",
              img(
                src = "https://solve-rd.eu/wp-content/uploads/2018/05/cnag-300x127.png",
                height = "200px"
              )
            )
          ),
          grid_place(
            area = "logo2",
            card_body(
              img(
                src = "https://www.ihi.europa.eu/sites/default/files/styles/teaser_image_horizontal/public/projects/logos/3TR_logo_final.jpg?itok=fDmi4lFR",
                height = "200px"
              )
            )
          ),
          grid_place(
            area = "logo3",
            card_body(
              img(
                src = "https://cnag-biomedical-informatics.github.io/pheno-ranker/img/3tr-funding.png",
                height = "80px"
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
      )
    )
  )
}

mod_landing_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
