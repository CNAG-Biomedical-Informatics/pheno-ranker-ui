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

mod_input_examples_page_ui <- function(id){
  ns <- NS(id)
  grid_container(
    layout = mode_input_examples_layout,
    gap_size = "0px",
    grid_place(
      area = "btn",
      actionButton(
        ns("RetrieveExampleCohorts"),
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
      mod_show_history_button_ui(ns("RetrievalHistorySidebar"))
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
  # phenoSimBin <- get_golem_options("PHENO_SIM_BIN")
  ouputFolder <- get_golem_options("inputExamplesOutputFolder")
  # ontologyUploadFolder <- get_golem_options("ontologyUploadFolder")

  fn <- paste0(
    ouputFolder,
    retrievalId,
    ".pxf.json"
  )

  # settings <- "-external-ontologies "

  # ontology_path <- paste0(ontologyUploadFolder, paste0(simulationId, "_ontologies.yaml "))
  # settings <- paste0(settings, ontology_path)
  # settings <- paste0(settings, ext_onts_settings_string)

  # print("settings")
  # print(settings)

  # # maybe better get it from a config file
  # flags <- c(
  #   "diseases", 
  #   "exposures", 
  #   "phenotypicFeatures", 
  #   "procedures", 
  #   "treatments"
  # )

  # external_ontologies_settings <- unlist(
  #   lapply(
  #     flags, 
  #     function(flag) 
  #     c(
  #       paste0("-", flag), 
  #       paste0("-max-", flag, "-pool")
  #     )
  #   )
  # )

  # indices <- seq(1, length(external_ontologies_settings))

  # # Loop through the settings and add them
  # settings_mapping <- list()
  # for (i in seq(1, length(indices), by = 2)) {
  #   ontology <- external_ontologies_settings[i]
  #   max_pool <- external_ontologies_settings[i + 1]

  #   ontology_count <- simSettings[indices[i]]
  #   max_pool_size <- simSettings[indices[i + 1]]

  #   settings <- paste0(
  #     settings, ontology, " ", ontology_count, " ",
  #     max_pool, " ", max_pool_size, " ")

  #   settings_mapping[[ontology]] <- ontology_count
  #   settings_mapping[[max_pool]] <- max_pool_size
  # }

  # number_of_individuals <- simSettings[11]
  # settings_mapping[["-n"]] <- number_of_individuals

  # print("settings_mapping")
  # print(settings_mapping)

  # settings <- paste0(
  #   settings,
  #   " -n ", number_of_individuals,
  #   " -f ", outputFormat,
  #   " -o ", fn
  # )

  # jq -s '.' $(ls -1 */*json | sort -R | head -10) > combined.json

  cmd <- paste(
    "jq -s '.' $(ls -1 */*json | sort -R | head -",
    number_of_individuals,
    ") > ",
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

  moduleServer(id, function(input, output, session){
    loader_inline <- addLoader$new(
      target_selector = "getCohort",
      color = "white",
      type = "ring",
      method = "inline"
    )

    mod_show_history_button_server(
      "InputExamplesRetrievalHistorySidebar",
      "input_examples",
      "InputEamplesRetrievalHistorySidebar",
      db_conn
    )

    iv <- InputValidator$new()
    iv$add_rule("arraySizeInput", sv_between(0, 5000))
    iv$enable()

    # data <- data.frame(
    #   ontologies = c(
    #     "diseases",
    #     "exposures",
    #     "phenotypicFeatures",
    #     "procedures",
    #     "treatments"
    #   ),
    #   count = c(3, 3, 3, 3, 3),
    #   max_pool_size = c(5, 5, 5, 5, 5)
    # )

    # Render the DataTable with input fields for count and max_pool_size
    # output$simulationSettings <- renderDT(
    #   data,
    #   selection = "none",
    #   options = list(
    #     paging = FALSE,
    #     searching = FALSE,
    #     ordering = FALSE,
    #     info = FALSE,
    #     autoWidth = FALSE,
    #     columnDefs = list(
    #       list(
    #         targets = c(2, 3),
    #         render = JS("returnNumberInputField")
    #       )
    #     )
    #   )
    # )

    retrieved_examples_folder <- get_golem_options(
      "retrievedInputExamplesFolder"
    )

    # simulationOutputFolder <- get_golem_options("simulationOutputFolder")

    observe({
      req(rv_input_examples$retrievalId)

      path <- paste0(
        retrieved_examples_folder,
        rv_input_examples$retrievalId
      )
      # bffOutputFn <- paste0(path, ".bff.json")
      pxfOutputFn <- paste0(path, ".pxf.json")

      print("bffOutputFn")
      print(bffOutputFn)

      # output$bffDl <- outputDownloadHandler(bffOutputFn, "phenoRankerSim.bff")
      output$pxfDl <- outputDownloadHandler(
        pxfOutputFn,
        "phenoRankerInputExamples.pxf"
      )
      # output$allDl <- outputDownloadHandler(
      #   list(bffOutputFn, pxfOutputFn), 
      #   list("phenoRankerSim.bff", "phenoRankerSim.pxf"),
      #   output_name = "phenoRankerSim.zip",
      #   zip_download = TRUE
      # )
    })

    # observeEvent(input$ontologiesFile, {
    #   req(input$ontologiesFile)

    #   allowed_types <- c("yaml", "yml")
    #   if (!(get_file_ext(input$ontologiesFile$name) %in% allowed_types)) {
    #     showNotification("Invalid file type!", type = "error")
    #     reset("ontologiesFile")
    #     return()
    #   }

    #   file_data <- paste(
    #     readLines(
    #       input$ontologiesFile$datapath
    #     ),
    #     collapse = "\n"
    #   )

    #   yamlValid <- validateYAML(file_data)
    #   if (yamlValid != "YAML is valid") {
    #     showNotification(yamlValid, type = "error")
    #     output$errorOutput <- renderText(yamlValid)
    #     updateAceEditor(session, "yamlEditor_diseases", value = "")
    #     updateAceEditor(session, "yamlEditor_phenos", value = "")
    #     updateAceEditor(session, "yamlEditor_treatments", value = "")
    #     updateAceEditor(session, "yamlEditor_procedures", value = "")
    #     updateAceEditor(session, "yamlEditor_expos", value = "")
    #     return()
    #   }
    #   output$errorOutput <- renderText(yamlValid)

    #   # update the ace editor
    #   yaml_data <- yaml.load(file_data)
    #   updateAceEditor(
    #     session,
    #     "yamlEditor_diseases",
    #     value = as.yaml(yaml_data$diseases)
    #   )

    #   updateAceEditor(
    #     session,
    #     "yamlEditor_phenos",
    #     value = as.yaml(yaml_data$phenotypicFeatures)
    #   )

    #   updateAceEditor(
    #     session,
    #     "yamlEditor_treatments",
    #     value = as.yaml(yaml_data$treatments)
    #   )

    #   updateAceEditor(
    #     session,
    #     "yamlEditor_procedures",
    #     value = as.yaml(yaml_data$procedures)
    #   )

    #   updateAceEditor(
    #     session,
    #     "yamlEditor_expos",
    #     value = as.yaml(yaml_data$exposures)
    #   )
    # })

    # editors <- c(
    #   "yamlEditor_diseases",
    #   "yamlEditor_phenos",
    #   "yamlEditor_treatments",
    #   "yamlEditor_procedures",
    #   "yamlEditor_expos"
    # )

    # TODO
    # add to rv_sim a yaml is valid flag
    # set it to false if one of the yaml is not valid
    # and display an error message
    # lapply(editors, function(editor) {
    #   observeEvent(input$editor, {
    #     result <- validateYAML(input$editor)
    #     output$errorOutput <- renderText(result)
    #   })
    # })
    
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

    # observeEvent(input$inputs, {
    #   rv_sim$dtInputs <- input$inputs
    # })

    observeEvent(input$retrieveBtnClicked, {
      req(iv$is_valid())
      # req(rv_sim$dtInputs)
      # simSettings <- c(rv_sim$dtInputs, input$arraySizeInput)

      # simulationId <- writeYAMLDataToFile(
      #   input$yamlEditor_diseases,
      #   input$yamlEditor_expos,
      #   input$yamlEditor_phenos,
      #   input$yamlEditor_procedures,
      #   input$yamlEditor_treatments
      # )

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
      }
      else {
        session$sendCustomMessage(
          type = "triggerWaitForElement",
          message = list(
            element = "span",
            text = "No preview available for more than 1000 individuals."
          )
        )
      }

      output$retrievalId <- renderText(paste0("RUN ID: ",retrievalId))
      
      # selectedOutputFormats <- input$checkboxes

      
      # create the external ontology setting string

      # maybe better get it from a config file
      # flags <- c(
      #   "diseases", 
      #   "exposures", 
      #   "phenotypicFeatures", 
      #   "procedures", 
      #   "treatments"
      # )

      # external_ontologies_settings <- unlist(
      #   lapply(
      #     flags, 
      #     function(flag) 
      #     c(
      #       paste0("-", flag), 
      #       paste0("-max-", flag, "-pool")
      #     )
      #   )
      # )

      # indices <- seq(1, length(external_ontologies_settings))

      # Loop through the settings and add them
      # ext_onts_settings_mapping <- list()
      # ext_onts_settings_string <- ""
      # for (i in seq(1, length(indices), by = 2)) {
      #   ontology <- external_ontologies_settings[i]
      #   max_pool <- external_ontologies_settings[i + 1]

      #   ontology_count <- simSettings[indices[i]]
      #   max_pool_size <- simSettings[indices[i + 1]]

      #   ext_onts_settings_string <- paste0(
      #     ext_onts_settings_string, ontology, " ", ontology_count, " ",
      #     max_pool, " ", max_pool_size, " ")

      #   ext_onts_settings_mapping[[ontology]] <- ontology_count
      #   ext_onts_settings_mapping[[max_pool]] <- max_pool_size
      # }

      # number_of_individuals <- simSettings[11]

      rv_input_examples$retrievedExamples <- get_input_examples(
        retrievalId, 
        number_of_individuals
      )
      

      # print("ext_onts_settings_string")
      # print(ext_onts_settings_string)

      # lapply(selectedOutputFormats, function(option) {
      #   if (option == "BFF") {
      #     rv_sim$simResult_bff <- simulate_data(
      #       "bff", 
      #       simulationId, 
      #       ext_onts_settings_string,
      #       number_of_individuals
      #     )
      #   } else if (option == "PXF") {
      #     rv_sim$simResult_pxf <- simulate_data(
      #       "pxf", 
      #       simulationId, 
      #       ext_onts_settings_string,
      #       number_of_individuals
      #     )
      #   }
      # })

      selectedOutputFormats <- "PXF"

      print("input$arraySizeInput")
      print(input$arraySizeInput)
      mod_json_viewer_server(
        ns("json_viewer"),
        selectedOutputFormats,
        rv_input_examples$retrievedExamples,
        rv_input_examples$retrievedExamples,
        # rv_sim$simResult_bff,
        # rv_sim$simResult_pxf,
        input$arraySizeInput
      )

      # rv_sim$simulationId <- simulationId
      rv_input_examples$retrievalId <- retrievalId

      settings <- list(
        # outputFormats = tolower(selectedOutputFormats),
        # externalOntologies = ext_onts_settings_mapping,
        numberOfIndividuals = number_of_individuals
      )

      # TODO
      # it would be good to have the
      # ids more self explanatory
      # simulationId <- paste0("sim",simulationId)

      # print("insert into db")
      # print("simulationId")
      # print(simulationId)
      # print("settings")
      # print(settings)
      # print("toJSON(settings)")
      # print(toJSON(settings))

      # label <- paste0("Simulation ID: ",simulationId)
      label <- paste0("Retrieval ID: ", retrievalId)
      # print("label")
      # print(label)

      # print ("db_driver")
      # print (db_driver)

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
        query_string <- "
          INSERT INTO jobs (run_id, user_id, mode, label, settings, status) 
          VALUES (%s,%s,'%s','%s','%s','%s')
        "
      }
      query <- sprintf(
        query_string,
        retrievalId, 1,"input_examples",label,settings_json,"success"
      )
      print("query")
      print(query)
      dbExecute(db_conn, query)
      click("InputExamplesRetrievalHistorySidebar-btn_show_history")
    })
  })
}


# TODO
# put this in fct_helpers.R
generateJsonView <- function(jsonOutput, title, width=12) {
  # TODO
  # figure out how to give the div the heigh 85vh

  print("generateJsonView")

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