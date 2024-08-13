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
        "exampleRequestTriggered"
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

      if (requested_individuals < 1000) {
        send_custom_message(session, "root")
      } else {
        send_custom_message(
          session,
          "No preview available for more than 1000 individuals."
        )
      }

      if (target_selector == "retrieveExampleCohorts") {
        js$exampleRequestTriggered()
      } else {
        js$getInputs()
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