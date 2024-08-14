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
      functions = c(
        "getInputs",
        "exampleRequestTriggered",
        "conversionStartTriggered"
      )
    )
  )
}

send_custom_message <- function(session, text) {
  session$sendCustomMessage(
    type = "triggerWaitForElement",
    message = list(
      element = "span",
      text = text
    )
  )
}

mod_loader_server <- function(
  id,
  session,
  target_selector,
  submit_clicked,
  title,
  sub_title,
  requested_individuals) {

  ns <- session$ns
  print("ns(target_selector)")
  print(ns(target_selector))

  moduleServer(id, function(input, output, session) {

    print("session$ns(elementFound)")
    print(session$ns("elementFound"))

    json_viewer_preview_limit <- get_golem_options("jsonViewerPreviewLimit")

    loader_inline <- addLoader$new(
      target_selector = paste0("#", ns(target_selector)),
      isID = FALSE,
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

      # TODO
      # should not be hardcoded but get_golem_options
      if (requested_individuals < json_viewer_preview_limit)  {
      # if (requested_individuals < 1000) {
        send_custom_message(session, "root")
      } else {
        send_custom_message(
          session,
          # TODO should not be hardcoded
          # "No preview available for more than 1000 individuals."
          paste(
            "No preview available for more than",
            json_viewer_preview_limit,
            "individuals."
          )
        )
      }

      if (target_selector == "retrieveExampleCohorts") {
        js$exampleRequestTriggered()
      } else if (target_selector == "simulateCohort") {
        js$getInputs()
      } else {
        js$conversionStartTriggered()
      }
    })

    # below is not working
    observeEvent(input$elementFound, {
      print("observeEvent(input$elementFound")
      loader_inline$hide()
      removeModal()
    })
  })
}