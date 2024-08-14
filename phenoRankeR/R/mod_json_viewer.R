#' Simulation Mode Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom listviewer renderReactjson reactjson
#' @import reactR
#' @importFrom shiny NS div renderUI fluidRow column span
#' @importFrom bslib card_header card_body
#' @noRd


mod_json_viewer_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("json_viewer"))
}

generateJsonView <- function(jsonOutput, title, width = 12) {
  # TODO
  # figure out how to give the div the heigh 85vh
  print("generateJsonView")

  # print("jsonOutput")
  # print(jsonOutput)

  column(
    width,
    height = "85vh",
    div(
      card_header(title),
      renderReactjson({
        reactjson(
          jsonOutput,
          onEdit = FALSE,
          onAdd = FALSE,
          onDelete = FALSE,
        )
      }),
      # reactjsonOutput(
      #   outputId = outputId,
      #   height = "85vh"
      # )
    )
  )
}

mod_json_viewer_server <- function(
  id,
  checkboxes,
  bff_out,
  pxf_out,
  arraySizeInput,
  conv_out = NULL
) {
  moduleServer(id, function(input, output, session) {

    preview_limit <- get_golem_options("jsonViewerPreviewLimit")
    print("preview_limit")
    print(preview_limit)

    if (arraySizeInput <= preview_limit) {
      if (is.null(checkboxes)) {
        output$json_viewer <- renderUI({
          generateJsonView(conv_out, "JSON output", 12)
        })
        return()
      }

      output$json_viewer <- renderUI({
        if (length(checkboxes) > 1) {
          fluidRow(
            generateJsonView(bff_out, "BFF", 6),
            generateJsonView(pxf_out, "PXF", 6)
          )
        } else if (checkboxes == "BFF") {
          fluidRow(
            generateJsonView(bff_out, "BFF")
          )
        } else if (checkboxes == "PXF") {
          fluidRow(
            generateJsonView(pxf_out, "PXF")
          )
        }
      })
    } else {
      print("The data is too large to be displayed here. ")
      output$json_viewer <- renderUI({
        fluidRow(
          column(
            width = 12,
            height = "85vh",
            span(
              paste(
                "No preview available for more than",
                preview_limit,
                "individuals."
              )
            )
          )
        )
      })
    }
  })
}
