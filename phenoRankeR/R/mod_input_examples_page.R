#' Simulation Mode Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom gridlayout grid_container grid_card grid_place
#' @importFrom shiny NS actionButton
#' @importFrom DT renderDT dataTableOutput JS
#' @importFrom shinyjs useShinyjs extendShinyjs click js reset
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom listviewer renderReactjson reactjson
#' @importFrom shinyvalidate InputValidator sv_between
#' @importFrom jsonlite read_json fromJSON toJSON
#' @importFrom spsComps addLoader
#' @import reactR
#' @noRd

mode_input_examples_layout <- c(
  "         500px       1fr     40px                           ",
  "30px     btn         examplesRes  btn_show_retrieval_history",
  "800px    table       examplesRes  btn_show_retrieval_history",
  "70px     download    examplesRes  btn_show_retrieval_history",
  "1px      version     version version                        "
)

mod_json_viewer_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("json_viewer"))
}

mod_input_examples_page_ui <- function(id) {
  ns <- NS(id)
  grid_container(
    layout = mode_input_examples_layout,
    gap_size = "0px",
    grid_place(
      area = "btn",
      actionButton(
        ns("retrieveExampleCohorts"),
        "Retrieve Example Cohorts",
        class = "btn btn-primary"
      )
    ),
    grid_card(
      area = "table",
      card_header("Options"),
      card_body(
        grid_container(
          layout = c(
            "       1fr           ",
            "85px   arraySizeInput"
          ),
          gap_size = "0px",
          grid_place(
            area = "arraySizeInput",
            numericInput(
              ns("arraySizeInput"),
              "Number of individuals:",
              value = 25,
              min = 1,
              max = 5000,
              step = 1
            )
          )
        )
      )
    ),
    grid_card(
      area = "download",
      card_body(
        grid_container(
          layout = c(
            "     1fr   ",
            "25px pxfDl "
          ),
          gap_size = "5px",
          grid_place(
            area = "pxfDl",
            downloadButton(ns("pxfDl"), "PXF")
          )
        )
      )
    ),
    grid_card(
      area = "examplesRes",
      card_header("Example input"),
      full_screen = TRUE,
      card_body(
        verbatimTextOutput("retrievalId"),
        mod_json_viewer_ui(ns("json_viewer"))
      ),
      height = "890px"
    ),
    grid_place(
      area = "btn_show_retrieval_history",
      mod_show_history_button_ui(ns("InputExamplesRetrievalHistorySidebar"))
    ),
    grid_place(
      area = "version",
      card_body(
        style = "text-align: right;",
        p("Version 0.0.0.9016")
      )
    )
  )
}

get_input_examples <- function(retrievalId, number_of_individuals) {
  inputFolder <- get_golem_options("inputExamplesInputFolder")
  ouputFolder <- get_golem_options("inputExamplesOutputFolder")

  json_files <- list.files(
    path = inputFolder,
    pattern = "\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )

  selected_files <- sample(
    json_files,
    number_of_individuals
  )

  fn <- paste0(
    ouputFolder,
    retrievalId,
    ".pxf.json"
  )

  # cmd <- paste0(
  #   "jq -s '.' $(ls -1 ",
  #   inputFolder,
  #   "*/*json | sort -R | head -",
  #   number_of_individuals,
  #   ") > ",
  #   fn
  # )

  cmd <- paste0(
    "jq -s '.' ",
    paste(selected_files, collapse = " "),
    " > ",
    fn
  )

  print(cmd)

  script_status <- system(cmd, intern = TRUE)
  if (length(script_status) > 0 && script_status != 0) {
    stop("bash script execution failed.")
  }
  return(read_json(fn))
}

mod_input_examples_page_server <- function(id, session, db_conn, db_driver, rv_input_examples) {
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <- session$ns

  moduleServer(id, function(input, output, session) {
    loader_inline <- addLoader$new(
      target_selector = "getCohort",
      color = "white",
      type = "ring",
      method = "inline"
    )

    mod_show_history_button_server(
      "InputExamplesRetrievalHistorySidebar",
      "input_examples",
      "InputExamplesRetrievalHistorySidebar",
      db_conn
    )

    iv <- InputValidator$new()
    iv$add_rule("arraySizeInput", sv_between(0, 5000))
    iv$enable()

    retrieved_examples_folder <- get_golem_options(
      "inputExamplesOutputFolder"
    )

    observe({
      req(rv_input_examples$retrievalId)

      path <- paste0(
        retrieved_examples_folder,
        rv_input_examples$retrievalId
      )
      pxfOutputFn <- paste0(path, ".pxf.json")

      output$pxfDl <- outputDownloadHandler(
        pxfOutputFn,
        "phenoRankerInputExamples.pxf"
      )
    })

    observeEvent(input$getExampleCohorts, {
      print("observeEvent(input$getExampleCohorts")
      loader_inline$show()
      showModal(
        modalDialog(
          title = "Example retrieval in progress",
          "Please wait while the retrieval is on going...",
          footer = NULL
        )
      )
      js$getInputs()
    })

    observeEvent(input$elementFound, {
      print("observeEvent(input$elementFound")
      loader_inline$hide()
      removeModal()
    })

    observeEvent(input$retrieveExampleCohorts, {
      print("observeEvent(input$retrieveExampleCohorts")
      req(iv$is_valid())

      retrievalId <- format(Sys.time(), "%Y%m%d%H%M%S")

      session$sendCustomMessage(
        type = "changeURL",
        message = list(
          mode = "input_examples",
          id = retrievalId
        )
      )

      if (input$arraySizeInput < 1000) {
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

      output$retrievalId <- renderText(paste0("RUN ID: ", retrievalId))

      rv_input_examples$inputExamples <- get_input_examples(
        retrievalId,
        input$arraySizeInput
      )

      selectedOutputFormats <- "PXF"
      number_of_individuals <- input$arraySizeInput

      mod_json_viewer_server(
        ns("json_viewer"),
        selectedOutputFormats,
        rv_input_examples$inputExamples,
        rv_input_examples$inputExamples,
        number_of_individuals
      )

      rv_input_examples$retrievalId <- retrievalId

      settings <- list(
        numberOfIndividuals = number_of_individuals
      )

      print("settings HERE")
      print(settings)

      # TODO
      # it would be good to have the
      # ids more self explanatory
      # simulationId <- paste0("sim",simulationId)

      label <- paste0("Retrieval ID: ", retrievalId)

      #* NOTE
      # JSONB is only available in sqlite > 3.45.0
      # planned for 2024-01-31

      settings_json <- toJSON(settings)
      print("settings_json")
      print(settings_json)

      query_string <- "
        INSERT INTO jobs (run_id, user_id, mode, label, settings, status)
        VALUES (%s,%s,'%s','%s',cast('%s' as JSONB),'%s')
      "

      if (db_driver == "SQLite") {
        print("db_driver SQLite")
        query_string <- "
          INSERT INTO jobs (run_id, user_id, mode, label, settings, status)
          VALUES (%s,%s,'%s','%s','%s','%s')
        "
      }

      query <- sprintf(
        query_string,
        retrievalId, 1, "input_examples", label, settings_json, "success"
      )
      print("query")
      print(query)
      dbExecute(db_conn, query)
      click("InputExamplesRetrievalHistorySidebar-btn_show_history")
    })
  })
}

mod_json_viewer_server <- function(id, checkboxes, bff_out, pxf_out, arraySizeInput) {
  moduleServer(id, function(input, output, session) {
    if (arraySizeInput <= 1000) {
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
              "No preview available for more than 1000 individuals."
            )
          )
        )
      })
    }
  })
}
