#' Loader Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom spsComps addLoader
#' @noRd

mod_loader_ui <- function(id) {
  ns <- NS(id)
  div(
    useShinyjs(),
    extendShinyjs(
      script = "www/handlers.js",
      functions = c("exampleRequestTriggered")
    )
  )
}

mod_loader_server <- function(id, session, submit_clicked, requested_individuals) {

  moduleServer(id, function(input, output, session) {

    title <- "Example Retrieval"
    sub_title <- "Please wait while the retrieval is ongoing..."

    loader_inline <- addLoader$new(
      target_selector = "retrieveExampleCohorts",
      color = "white",
      type = "ring",
      method = "inline"
    )

    observeEvent(submit_clicked(), {
      loader_inline$show()
      showModal(
        modalDialog(
          title = title,
          sub_title,
          footer = NULL
        )
      )

      if (requested_individuals < 1000) {
        session$sendCustomMessage(
          type = "triggerWaitForElement",
          message = list(
            element = "span",
            text = "root"
          )
        )
      } else {
        session$sendCustomMessage(
          type = "triggerWaitForElement",
          message = list(
            element = "span",
            text = "No preview available for more than 1000 individuals."
          )
        )
      }
      js$exampleRequestTriggered()
    })

    observeEvent(input$elementFound, {
      print("observeEvent(input$elementFound")
      loader_inline$hide()
      removeModal()
    })
  })
}