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
        "conversionStartTriggered",
        "cohortRankingStartTriggered",
        "patientRankingStartTriggered"
      )
    )
  )
}

send_custom_message <- function(session, text, element = "span") {
  session$sendCustomMessage(
    type = "triggerWaitForElement",
    message = list(
      element = element,
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
    print("json_viewer_preview_limit")
    print(json_viewer_preview_limit)

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
      # in order to make this work it is necessary
      # to always switch the tab to the heatmap tab

      if (is.null(requested_individuals)) {
        send_custom_message(
          session,
          "Initialize the original heatmap.",
          element = "div.shiny-notification-content-text"
        )
        if (target_selector == "rankPatient") {
          js$patientRankingStartTriggered()
        } else {
          js$cohortRankingStartTriggered()
        }
        return()
      }

      if (requested_individuals < json_viewer_preview_limit)  {
        send_custom_message(session, "root")
      } else {
        send_custom_message(
          session,
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