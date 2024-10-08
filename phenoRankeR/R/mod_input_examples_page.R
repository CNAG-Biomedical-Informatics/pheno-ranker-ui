#' Simulation Mode Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom gridlayout grid_container grid_card grid_place new_gridlayout
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

get_examples_util_layout <- new_gridlayout(
  c(
    "btn examplesRes btn_show_retrieval_history",
    "table examplesRes btn_show_retrieval_history",
    "download examplesRes btn_show_retrieval_history",
    "version version version"
  ),
  col_sizes = c("500px", "1fr", "40px"),
  alternate_layouts = list(
    layout = c(
      "         320px        1fr          40px                     ",
      "30px     btn         examplesRes  btn_show_retrieval_history",
      "800px    table       examplesRes  btn_show_retrieval_history",
      "70px     download    examplesRes  btn_show_retrieval_history",
      "1px      version     version      version                   "
    ),
    width_bounds = c(max = 1100)
  )
)


mod_input_examples_page_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")
  max_individuals <- get_golem_options("maxIndividuals")

  grid_container(
    layout = get_examples_util_layout,
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
            "85px   exampleSource ",
            "85px   sourceInfo     ",
            "85px   textInput     ",
            "85px   arraySizeInput"
          ),
          gap_size = "0px",
          grid_place(
            area = "exampleSource",
            selectInput(
              ns("exampleSource"),
              "Example source:",
              c("phenopacket-store")
            )
          ),
          grid_place(
            area = "sourceInfo",
            div(
              p("Info about the selected example source:"),
              a("
                Phenopacket store",
                href = "https://monarch-initiative.github.io/phenopacket-store/collections",
                target = "_blank"
              )
            )
          ),
          grid_place(
            area = "textInput",
            textInput(
              ns("textInput"),
              "Cohort names of interest:",
              value = "CWC27,DHCR24"
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
                ns("loader_example_retrieval")
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
      card_header("Example input"),
      full_screen = TRUE,
      card_body(
        verbatimTextOutput("retrievalId"),
        mod_json_viewer_ui(ns("json_viewer_input_examples"))
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
        p("Version ", version)
      )
    )
  )
}

get_input_examples <- function(retrievalId, number_of_individuals, cohort_names, rv_general) {
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
    retrievalId,
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

mod_input_examples_page_server <- function(
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
      input$retrieveExampleCohorts
    })

    mod_loader_server(
      ns("loader_example_retrieval"),
      session,
      "retrieveExampleCohorts",
      submit_clicked,
      "Example Retrieval",
      "Please wait while the retrieval is ongoing...",
      input$arraySizeInput
    )

    mod_show_history_button_server(
      "InputExamplesRetrievalHistorySidebar",
      "input_examples",
      "InputExamplesRetrievalHistorySidebar",
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
      req(rv_input_examples$retrievalId)

      path <- paste0(
        rv_general$user_dirs$output$examples,
        "/",
        # retrieved_examples_folder,
        rv_input_examples$retrievalId
      )
      pxfOutputFn <- paste0(path, ".pxf.json")

      output$pxfDl <- outputDownloadHandler(
        pxfOutputFn,
        "phenoRankerInputExamples.pxf"
      )
    })

    observeEvent(input$getExampleInputClicked, {
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

      output$retrievalId <- renderText(paste0("RUN ID: ", retrievalId))

      rv_input_examples$inputExamples <- get_input_examples(
        retrievalId,
        input$arraySizeInput,
        input$textInput,
        rv_general
      )

      selectedOutputFormats <- "PXF"
      number_of_individuals <- input$arraySizeInput

      mod_json_viewer_server(
        ns("json_viewer_input_examples"),
        selectedOutputFormats,
        rv_input_examples$inputExamples,
        rv_input_examples$inputExamples,
        number_of_individuals
      )

      rv_input_examples$retrievalId <- retrievalId

      settings <- list(
        numberOfIndividuals = number_of_individuals
      )

      # TODO
      # it would be good to have the
      # ids more self explanatory
      # simulationId <- paste0("sim",simulationId)

      label <- paste0("Retrieval ID: ", retrievalId)

      #* NOTE
      # JSONB is only available in sqlite > 3.45.0
      # planned for 2024-01-31

      store_job_in_db(
        retrievalId,
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
      #   retrievalId, 1, "input_examples", label, settings_json, "success"
      # )
      # print("query")
      # print(query)
      # dbExecute(db_conn, query)
      click("InputExamplesRetrievalHistorySidebar-btn_show_history")
    })
  })
}
