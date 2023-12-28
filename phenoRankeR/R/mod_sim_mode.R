#' Simulation Mode Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom gridlayout grid_container grid_card grid_place
#' @importFrom shiny NS actionButton
#' @importFrom DT renderDT dataTableOutput JS
#' @importFrom shinyjs useShinyjs extendShinyjs click js
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom listviewer renderReactjson reactjson
#' @importFrom shinyvalidate InputValidator sv_between
#' @importFrom jsonlite read_json fromJSON
#' @importFrom jsonlite toJSON
#' @import reactR
#' @noRd

mode_sim_layout <- c(
  "         500px       1fr    40px                ",
  "30px     btn         simRes btn_show_sim_history",
  "800px    table       simRes btn_show_sim_history",
  "70px     download    simRes btn_show_sim_history"
)

mod_json_viewer_ui <- function(id) {
  ns <- NS(id)
  uiOutput(
    ns("json_viewer")
  )
}

mod_sim_mode_ui <- function(id){
  ns <- NS(id)
  grid_container(
    layout = mode_sim_layout,
    gap_size = "0px",
    grid_place(
      area = "btn",
      actionButton(
        ns("simulateCohort"), 
        "Simulate", 
        class = "btn btn-primary"
      )
    ),
    grid_card(
      area = "table",
      card_header("Options"),
      card_body(
        grid_container(
          layout = c(
            "         1fr         1fr           ",
            "85px     checkboxes  arraySizeInput",
            "620px    onts        onts          "
          ),
          gap_size = "0px",
          grid_place(
            area = "checkboxes",
            checkboxGroupInput(
              ns("checkboxes"), 
              "Select output(s):",
              choices = c("BFF", "PXF"),
              selected = c("BFF", "PXF"),
              inline = TRUE
            )
          ),
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
          ),
          grid_place(
            area = "onts",
            div(
              tags$head(
                tags$style(
                  HTML(".shiny-notification {
                    position:fixed;
                    top: calc(15%);
                    left: calc(20.5%);
                    }
                ")
                )
              ),
              fileInput(
                ns("ontologiesFile"), 
                "Either upload an ontologies yaml file",
                multiple = FALSE,
                accept = c(
                  ".yaml"
                )
              ),
              span("or modify the ontologies below:"),
              tabsetPanel(
                selected = "diseases",
                tabPanel(
                  title = "diseases",
                  aceEditor(
                    ns("yamlEditor_diseases"),
                    value = onts_defaults_diseases,
                    mode = "yaml",
                    theme = "github",
                    height = "200px"
                  )
                ),
                tabPanel(
                  title = "phenotypicFeatures",
                  aceEditor(
                    ns("yamlEditor_phenos"),
                    value = onts_defaults_phenos,
                    mode = "yaml",
                    theme = "github",
                    height = "200px"
                  )
                ),
                tabPanel(
                  title = "treatments",
                  aceEditor(
                    ns("yamlEditor_treatments"),
                    value = onts_defaults_treatments,
                    mode = "yaml",
                    theme = "github",
                    height = "200px"
                  )
                )
              ),
              verbatimTextOutput(ns("errorOutput")),
              useShinyjs(),
              extendShinyjs(
                script = "www/handlers.js",
                functions = c("getInputs")
              ),
              div(
                class = "grid-item-table", 
                dataTableOutput(ns("simulationSettings"))
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
            "     1fr   1fr   1fr   ",
            "25px  bffDl pxfDl allDl "
          ),
          gap_size = "5px",
          grid_place(
            area = "bffDl",
            downloadButton(ns("bffDl"), "BFF")
          ),
          grid_place(
            area = "pxfDl",
            downloadButton(ns("pxfDl"), "PXF")
          ),
          grid_place(
            area = "allDl",
            downloadButton(ns("allDl"), "All")
          )
        )
      )
    ),
    grid_card(
      area = "simRes",
      card_header("Simulation Results"),
      full_screen = TRUE,
      card_body(
        verbatimTextOutput("simulationId"),
        mod_json_viewer_ui(ns("json_viewer"))
      ),
      height = "890px"
    ),
    grid_place(
      area = "btn_show_sim_history",
      mod_show_history_button_ui(ns("SimulateHistorySidebar"))
    )
  )
}

loadOntologyDefaults <- function(file_path, ontology) {
  if (file.exists(file_path)) {
    data <- paste(
      readLines(file_path),
      collapse = "\n"
    )
    data <- yaml.load(data)
    return(as.yaml(data[[ontology]]))
  } else {
    return(NULL)
  }
}

onts_defaults_diseases <- loadOntologyDefaults(
  "inst/extdata/defaults/diseases_onts.yaml",
  "diseases"
)

onts_defaults_phenos <- loadOntologyDefaults(
  "inst/extdata/defaults/phenos_onts.yaml",
  "phenotypicFeatures"
)

onts_defaults_treatments <- loadOntologyDefaults(
  "inst/extdata/defaults/treatments_onts.yaml",
  "treatments"
)

writeYAMLDataToFile <- function(yaml_diseases, yaml_phenos, yaml_treatments) {
  yaml_data <- list()
  yaml_data$diseases <- yaml.load(yaml_diseases)
  yaml_data$phenotypicFeatures <- yaml.load(yaml_phenos)
  yaml_data$treatments <- yaml.load(yaml_treatments)

  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  # file_path <- file.path("./data/uploads/ontologies", paste0(timestamp, "_ontologies.yaml"))
  file_path <- file.path(
    get_golem_options("ontologyUploadFolder"), 
    paste0(timestamp, "_ontologies.yaml")
  )
  writeLines(as.yaml(yaml_data), file_path)
  return(timestamp)
}

simulate_data <- function(outputFormat, simulationId, simSettings) {
  # cfg <- fromJSON(readLines("config/cfg.json"))
  # phenoSimBin <- cfg$PHENO_SIM_BIN
  phenoSimBin <- get_golem_options("PHENO_SIM_BIN")
  # ouputFolder <- cfg$simulationOutputFolder
  ouputFolder <- get_golem_options("simulationOutputFolder")
  # ontologyUploadFolder <- cfg$ontologyUploadFolder
  ontologyUploadFolder <- get_golem_options("ontologyUploadFolder") 

  fn <- paste0(
    ouputFolder,
    simulationId,
    ".",
    outputFormat,
    ".json"
  )

  settings_diseases <- paste0(
    "-diseases ", simSettings[1],
    " -max-diseases-pool ", simSettings[2]
  )

  settings_phenos <- paste0(
    "-phenotypicFeatures ", simSettings[3],
    " -max-phenotypicFeatures-pool ", simSettings[4]
  )

  settings_treatments <- paste0(
    "-treatments ", simSettings[5],
    " -max-treatments-pool ", simSettings[6]
  )

  #normalizePath probably not needed

  settings <- paste0(
    " -external-ontologies ",
    normalizePath(paste0(
      ontologyUploadFolder,
      simulationId,
      "_ontologies.yaml "
    )),
    settings_diseases,
    " ",
    settings_phenos,
    " ",
    settings_treatments,
    " -n ",
    simSettings[7],
    " -f ", outputFormat,
    " -o ", fn
  )

  cmd <- paste0(
    phenoSimBin,
    settings
  )
  print(cmd)

  script_status <- system(cmd, intern = TRUE)
  if (length(script_status) > 0 && script_status != 0) {
    stop("Perl script execution failed.")
  }
  return(read_json(fn))
}

mod_sim_mode_server <- function(id, session, db_conn, db_driver, rv_sim){
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <-session$ns
  moduleServer(id,function(input, output, session){
    mod_show_history_button_server(
      "SimulateHistorySidebar",
      "sim",
      "SimulateHistorySidebar",
      db_conn
    )

    iv <- InputValidator$new()
    iv$add_rule("arraySizeInput", sv_between(0, 5000))
    iv$enable()

    data <- data.frame(
      ontologies = c("diseases", "phenotypicFeatures", "treatments"),
      count = c(3, 3, 3),
      max_pool_size = c(5, 5, 5)
    )

    # Render the DataTable with input fields for count and max_pool_size
    output$simulationSettings <- renderDT(
      data,
      selection = "none",
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = c(2, 3),
            render = JS("returnNumberInputField")
          )
        )
      )
    )

    output$bffDl <- outputDownloadHandler(rv_sim$simResult_bff, "phenoRankerSim.bff")
    output$pxfDl <- outputDownloadHandler(rv_sim$simResult_pxf, "phenoRankerSim.pxf")
    output$allDl <- outputDownloadHandler(
      list(rv_sim$simResult_bff, rv_sim$simResult_pxf), 
      list("phenoRankerSim.bff", "phenoRankerSim.pxf"), 
      zip_download = TRUE
    )

    observeEvent(input$ontologiesFile, {
      req(input$ontologiesFile)

      allowed_types <- c("yaml", "yml")
      # file_ext <- tools::file_ext(input$ontologiesFile$name)
      if (!(get_file_ext(input$ontologiesFile$name) %in% allowed_types)) {
        showNotification("Invalid file type!", type = "error")
        reset("ontologiesFile")
        return()
      }

      file_data <- paste(
        readLines(
          input$ontologiesFile$datapath
        ),
        collapse = "\n"
      )

      yamlValid <- validateYAML(file_data)
      if (yamlValid != "YAML is valid") {
        showNotification(yamlValid, type = "error")
        output$errorOutput <- renderText(yamlValid)
        updateAceEditor(session, "yamlEditor_diseases", value = "")
        updateAceEditor(session, "yamlEditor_phenos", value = "")
        updateAceEditor(session, "yamlEditor_treatments", value = "")
        return()
      }
      output$errorOutput <- renderText(yamlValid)

      # update the ace editor
      yaml_data <- yaml.load(file_data)
      updateAceEditor(
        session,
        "yamlEditor_diseases",
        value = as.yaml(yaml_data$diseases)
      )

      updateAceEditor(
        session,
        "yamlEditor_phenos",
        value = as.yaml(yaml_data$phenotypicFeatures)
      )

      updateAceEditor(
        session,
        "yamlEditor_treatments",
        value = as.yaml(yaml_data$treatments)
      )
    })

    editors <- c(
      "yamlEditor_diseases",
      "yamlEditor_phenos",
      "yamlEditor_treatments"
    )

    # TODO
    # add to rv_sim a yaml is valid flag
    # set it to false if one of the yaml is not valid
    # and display an error message
    lapply(editors, function(editor) {
      observeEvent(input$editor, {
        result <- validateYAML(input$editor)
        output$errorOutput <- renderText(result)
      })
    })
    
    observeEvent(input$simulateCohort, {
      print("observeEvent(input$simulateCohort")
      js$getInputs()
    })

    observeEvent(input$inputs, {
      rv_sim$dtInputs <- input$inputs
    })

    observeEvent(input$simulateBtnClicked, {
      req(iv$is_valid())
      req(rv_sim$dtInputs)
      simSettings <- c(rv_sim$dtInputs, input$arraySizeInput)

      simulationId <- writeYAMLDataToFile(
        input$yamlEditor_diseases,
        input$yamlEditor_phenos,
        input$yamlEditor_treatments
      )

      session$sendCustomMessage(type = "changeURL", message = list(mode="sim",id=simulationId))
      output$simulationId <- renderText(paste0("RUN ID: ",simulationId))
      
      selectedOutputFormats <- input$checkboxes

      lapply(selectedOutputFormats, function(option) {
        if (option == "BFF") {
          rv_sim$simResult_bff <- simulate_data(
            "bff", 
            simulationId, 
            simSettings
          )
        } else if (option == "PXF") {
          rv_sim$simResult_pxf <- simulate_data(
            "pxf", 
            simulationId, 
            simSettings
          )
        }
      })

      mod_json_viewer_server(
        ns("json_viewer"), 
        selectedOutputFormats,
        rv_sim$simResult_bff,
        rv_sim$simResult_pxf
      )

      rv_sim$simulationId <- simulationId

      settings <- list(
        outputFormats = tolower(selectedOutputFormats)
      )

      # TODO
      # it would be good to have the
      # ids more self explanatory
      # simulationId <- paste0("sim",simulationId)

      print("insert into db")
      print("simulationId")
      print(simulationId)
      print("settings")
      print(settings)
      print("toJSON(settings)")
      print(toJSON(settings))
  
      label <- paste0("Simulation ID: ",simulationId)
      print("label")
      print(label)

      print ("db_driver")
      print (db_driver)

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
        simulationId, 1,"sim",label,settings_json,"success"
      )
      print("query")
      print(query)
      dbExecute(db_conn, query)
      click("SimulateHistorySidebar-btn_show_history")
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

mod_json_viewer_server <- function(id, checkboxes, bff_out, pxf_out) {
  moduleServer(id, function(input, output, session) {
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
  })
}