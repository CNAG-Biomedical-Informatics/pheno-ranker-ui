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
#' @importFrom httr2 request req_perform resp_body_json
#' @import reactR
#' @noRd

mode_beacon_api_layout <- c(
  "         500px       1fr     40px                           ",
  "30px     btn         beaconRes  btn_show_beacon_query_history",
  "800px    table       beaconRes  btn_show_beacon_query_history",
  "70px     download    beaconRes  btn_show_beacon_query_history",
  "1px      version     version version                        "
)

mod_beacon_api_page_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")
  max_individuals <- get_golem_options("maxIndividuals")

  grid_container(
    layout = mode_beacon_api_layout,
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
              c("beacon.biodata.pt")
            )
          ),
          grid_place(
            area = "sourceInfo",
            div(
              p("Info about the selected beacon:"),
              a("
                beacon.biodata.pt",
                href = "https://beacon.biodata.pt",
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
            "25px bffDl "
          ),
          gap_size = "5px",
          grid_place(
            area = "bffDl",
            downloadButton(ns("bffDl"), "BFF")
          )
        )
      )
    ),
    grid_card(
      area = "beaconRes",
      card_header("Query Results"),
      full_screen = TRUE,
      card_body(
        verbatimTextOutput("queryId"),
        mod_json_viewer_ui(ns("json_viewer_beacon_api"))
      ),
      height = "890px"
    ),
    grid_place(
      area = "btn_show_beacon_query_history",
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

  print("inside query_beacon_api")

  beacon <- "beacon.biodata.pt"
  # beacon <- "beacon-spain.ega-archive.org" # not working
  endpoint <- "api/individuals"

  url <- paste0("https://", beacon, "/", endpoint)

  req <- request(url)
  print("req")
  print(req)

  res <- req_perform(
    req,
    verbosity = 1
  )

  print("res")
  print(res)

  resp_body_json <- resp_body_json(res)

  print("names(resp_body_json)")
  print(names(resp_body_json))

  res_content <- NULL
  if ("response" %in% names(resp_body_json)) {
    res_content <- resp_body_json$response
  } else {
    # throw error
  }

  # print("res_content")
  # print(res_content)

  return(res_content)
}

process_beacon_api_response <- function(queryId, res_content, rv_general) {
  print("inside process_beacon_api_results")
  print(names(res_content))

  if ("resultSets" %in% names(res_content)) {
    print("names(res_content$resultSets[[1]])")
    print(names(res_content$resultSets[[1]]))

    # get the results from the first result set
    results <- res_content$resultSets[[1]]$results
    # print("results")
    # print(results)

    # results <- res_content$resultSets[1]$results
  } else {
    # throw error
  }

  # store the results in a file
  fn <- paste0(
    queryId,
    ".bff.json"
  )

  beaconApiOutputFolder <- rv_general$user_dirs$output$beacon

  write(
    toJSON(results, pretty = TRUE, auto_unbox = TRUE),
    file = paste0(beaconApiOutputFolder, "/", fn)
  )

  print("results")
  print(results)

  return(results)
}

get_input_examples <- function(queryId, number_of_individuals, cohort_names, rv_general) {
  inputFolder <- get_golem_options("inputExamplesInputFolder")
  # ouputFolder <- get_golem_options("inputExamplesOutputFolder")

  ouputFolder <- rv_general$user_dirs$output$beacon

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
    rv_beacon_api,
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
      "Please wait while the query is ongoing...",
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
        rv_general$user_dirs$output$beacon,
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

      response <- query_beacon_api(
        input$beaconSelector,
        input$textInput,
        input$arraySizeInput
      )

      rv_beacon_api$beaconApiResults <- process_beacon_api_response(
        queryId,
        response,
        rv_general
      )

      # rv_input_examples$inputExamples <- get_input_examples(
      #   queryId,
      #   input$arraySizeInput,
      #   input$textInput,
      #   rv_general
      # )

      selectedOutputFormats <- "BFF"
      number_of_individuals <- input$arraySizeInput

      mod_json_viewer_server(
        ns("json_viewer_beacon_api"),
        selectedOutputFormats,
        rv_beacon_api$beaconApiResults,
        rv_beacon_api$beaconApiResults,
        number_of_individuals
      )

      rv_beacon_api$queryId <- queryId

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

      print("before store_job_in_db")
      store_job_in_db(
        queryId,
        rv_general$user_email,
        "beacon_api",
        label,
        settings,
        db_conn
      )
      print("after store_job_in_db")

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
