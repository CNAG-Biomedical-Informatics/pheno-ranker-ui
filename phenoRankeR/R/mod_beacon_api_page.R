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
#' @import reactR
#' @noRd

mode_input_examples_layout <- c(
  "         500px       1fr     40px                           ",
  "30px     btn         examplesRes  btn_show_retrieval_history",
  "800px    table       examplesRes  btn_show_retrieval_history",
  "70px     download    examplesRes  btn_show_retrieval_history",
  "1px      version     version version                        "
)


mod_beacon_api_page_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")
  max_individuals <- get_golem_options("maxIndividuals")

  grid_container(
    layout = mode_input_examples_layout,
    gap_size = "0px",
    grid_place(
      area = "btn",
      actionButton(
        ns("queryBeaconApi"),
        "Query Beacon API",
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
            "85px   beaconSelector ",
            "85px   sourceInfo     ",
            "85px   textInput     ",
            "85px   arraySizeInput"
          ),
          gap_size = "0px",
          grid_place(
            area = "beaconSelector",
            selectInput(
              ns("beaconSelector"),
              "Public beacon:",
              c("beacon-spain.ega")
            )
          ),
          grid_place(
            area = "sourceInfo",
            div(
              p("Info about the selected beacon:"),
              a("
                beacon-spain.ega",
                href = "https://beacon.ega-archive.org",
                target = "_blank"
              )
            )
          ),
          grid_place(
            area = "textInput",
            textInput(
              ns("textInput"),
              "Dataset id of interest:",
              value = "EGAD00001008392"
            )
          ),
          grid_place(
            area = "arraySizeInput",
            div(
              numericInput(
                ns("arraySizeInput"),
                "Number of individuals:",
                value = 25,
                min = 1,
                max = max_individuals,
                step = 1
              ),
              mod_loader_ui(
                ns("loader_beacon_api")
              )
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
      card_header("Query Results"),
      full_screen = TRUE,
      card_body(
        verbatimTextOutput("queryId"),
        mod_json_viewer_ui(ns("json_viewer_input_examples"))
      ),
      height = "890px"
    ),
    grid_place(
      area = "btn_show_retrieval_history",
      mod_show_history_button_ui(ns("BeaconApiHistorySidebar"))
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

query_beacon_api <- function(queryId, beacon, datasetId, number_of_individuals) {
  # working APIs:
  # https://beacon-spain.ega-archive.org
  # https://staging-beacon.gdi.nbis.se
  # https://beacon.biodata.pt

  # endpoint: /api/individuals

  beacon <- "beacon.biodata.pt"
  beacon <- "beacon-spain.ega-archive.org"
  endpoint <- "api/individuals"

  url <- paste0("https://", beacon, "/", endpoint)

  req <- request(url)

  res <- req_perform(
    req,
    verbosity = TRUE
  )

  res_content <- req_content(res, as = "text")




  return(query)
}

get_input_examples <- function(queryId, number_of_individuals, cohort_names, rv_general) {
  inputFolder <- get_golem_options("inputExamplesInputFolder")
  # ouputFolder <- get_golem_options("inputExamplesOutputFolder")

  ouputFolder <- rv_general$user_dirs$output$examples

  json_files <- list.files(
    path = inputFolder,
    pattern = "\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(cohort_names) > 0) {
    cohort_names <- strsplit(cohort_names, ",")[[1]]
    json_files <- json_files[grep(paste(cohort_names, collapse = "|"), json_files)]
    # print("json_files")
    # print(json_files)
  }

  files_count <- length(json_files)

  if (files_count == 0) {
    return()
  }

  if (files_count < number_of_individuals) {
    number_of_individuals <- files_count
  }

  selected_files <- sample(
    json_files,
    number_of_individuals
  )

  fn <- paste0(
    ouputFolder,
    "/",
    queryId,
    ".pxf.json"
  )

  json_list <- lapply(selected_files, function(x) fromJSON(x))
  combined_json <- toJSON(
    json_list,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  write(combined_json, file = fn)
  return(combined_json)
}

mod_beacon_api_page_server <- function(
    id,
    session,
    db_conn,
    db_driver,
    rv_input_examples,
    rv_general) {
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <- session$ns
  max_individuals <- get_golem_options("maxIndividuals")

  moduleServer(id, function(input, output, session) {
    submit_clicked <- reactive({
      input$queryBeaconApi
    })

    mod_loader_server(
      ns("loader_beacon_api"),
      session,
      "queryBeaconApi",
      submit_clicked,
      "Beacon API Query",
      "Please wait while the queryz is ongoing...",
      input$arraySizeInput
    )

    mod_show_history_button_server(
      "BeaconApiHistorySidebar",
      "input_examples",
      "BeaconApiHistorySidebar",
      db_conn,
      rv_general$user_email
    )

    iv <- InputValidator$new()
    iv$add_rule("arraySizeInput", sv_between(0, max_individuals))
    iv$enable()

    # retrieved_examples_folder <- get_golem_options(
    #   "inputExamplesOutputFolder"
    # )

    observe({
      req(rv_beacon_api$queryId)

      path <- paste0(
        rv_general$user_dirs$output$examples,
        "/",
        # retrieved_examples_folder,
        rv_beacon_api$queryId
      )
      bffOutputFn <- paste0(path, ".bff.json")

      output$bffDl <- outputDownloadHandler(
        bffOutputFn,
        "beacon_api_results.bff"
      )
    })

    observeEvent(input$queryBeaconApiClicked, {
      print("observeEvent(input$queryBeaconApiClicked")
      req(iv$is_valid())

      queryId <- format(Sys.time(), "%Y%m%d%H%M%S")
      session$sendCustomMessage(
        type = "changeURL",
        message = list(
          mode = "beacon_api",
          id = queryId
        )
      )

      output$queryId <- renderText(paste0("RUN ID: ", queryId))

      rv_beacon_api$queryId <-
        rv_input_examples$inputExamples <- get_input_examples(
          queryId,
          input$arraySizeInput,
          input$textInput,
          rv_general
        )

      selectedOutputFormats <- "BFF"
      number_of_individuals <- input$arraySizeInput

      mod_json_viewer_server(
        ns("json_viewer_input_examples"),
        selectedOutputFormats,
        rv_input_examples$inputExamples,
        rv_input_examples$inputExamples,
        number_of_individuals
      )

      rv_input_examples$queryId <- queryId

      settings <- list(
        numberOfIndividuals = number_of_individuals
      )

      # TODO
      # it would be good to have the
      # ids more self explanatory
      # simulationId <- paste0("sim",simulationId)

      label <- paste0("Query ID: ", queryId)

      #* NOTE
      # JSONB is only available in sqlite > 3.45.0
      # planned for 2024-01-31

      store_job_in_db(
        queryId,
        rv_general$user_email,
        "input_examples",
        label,
        settings,
        db_conn
      )

      # settings_json <- toJSON(settings)
      # # print("settings_json")
      # # print(settings_json)

      # query_string <- "
      #   INSERT INTO jobs (run_id, user_id, mode, label, settings, status)
      #   VALUES (%s,%s,'%s','%s',cast('%s' as JSONB),'%s')
      # "

      # if (db_driver == "SQLite") {
      #   print("db_driver SQLite")
      #   query_string <- "
      #     INSERT INTO jobs (run_id, user_id, mode, label, settings, status)
      #     VALUES (%s,%s,'%s','%s','%s','%s')
      #   "
      # }

      # query <- sprintf(
      #   query_string,
      #   queryId, 1, "input_examples", label, settings_json, "success"
      # )
      # print("query")
      # print(query)
      # dbExecute(db_conn, query)
      click("BeaconApiHistorySidebar-btn_show_history")
    })
  })
}
