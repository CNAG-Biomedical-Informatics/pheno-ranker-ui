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


mode_sim_layout <- c(
  "         500px       1fr     40px                ",
  "30px     btn         simRes  btn_show_sim_history",
  "800px    table       simRes  btn_show_sim_history",
  "70px     download    simRes  btn_show_sim_history",
  "1px      version     version version             "
)

mod_sim_mode_ui <- function(id) {
  ns <- NS(id)
  max_individuals <- get_golem_options("maxIndividuals")
  version <- get_golem_options("packageVersion")

  grid_container(
    layout = mode_sim_layout,
    gap_size = "0px",
    grid_place(
      area = "btn",
      actionButton(
        ns("simulateCohort"),
        " Simulate",
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
              max = max_individuals,
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
                  title = "exposures",
                  aceEditor(
                    ns("yamlEditor_expos"),
                    value = onts_defaults_phenos,
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
                  title = "procedures",
                  aceEditor(
                    ns("yamlEditor_procedures"),
                    value = onts_defaults_procedures,
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
              mod_loader_ui(ns("loader_simulate")),
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
        mod_json_viewer_ui(ns("json_viewer_sim_mode"))
      ),
      height = "890px"
    ),
    grid_place(
      area = "btn_show_sim_history",
      mod_show_history_button_ui(ns("SimulateHistorySidebar"))
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

onts_defaults_procedures <- loadOntologyDefaults(
  "inst/extdata/defaults/procedures_onts.yaml",
  "procedures"
)

onts_defaults_exposures <- loadOntologyDefaults(
  "inst/extdata/defaults/exposures_onts.yaml",
  "exposures"
)

writeYAMLDataToFile <- function(
    yaml_diseases,
    yaml_expos,
    yaml_phenos,
    yaml_procedures,
    yaml_treatments) {
  yaml_data <- list()
  yaml_data$diseases <- yaml.load(yaml_diseases)
  yaml_data$exposures <- yaml.load(yaml_expos)
  yaml_data$phenotypicFeatures <- yaml.load(yaml_phenos)
  yaml_data$procedures <- yaml.load(yaml_procedures)
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

simulate_data <- function(outputFormat, simulationId, ext_onts_settings_string, number_of_individuals) {
  phenoSimBin <- get_golem_options("PHENO_SIM_BIN")
  ouputFolder <- get_golem_options("simulationOutputFolder")
  ontologyUploadFolder <- get_golem_options("ontologyUploadFolder")

  fn <- paste0(
    ouputFolder,
    simulationId,
    ".",
    outputFormat,
    ".json"
  )

  settings <- "-external-ontologies "

  ontology_path <- paste0(ontologyUploadFolder, paste0(simulationId, "_ontologies.yaml "))
  settings <- paste0(settings, ontology_path)
  settings <- paste0(settings, ext_onts_settings_string)

  print("settings")
  print(settings)

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

  settings <- paste0(
    settings,
    " -n ", number_of_individuals,
    " -f ", outputFormat,
    " -o ", fn
  )

  cmd <- paste(
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

mod_sim_mode_server <- function(id, session, db_conn, db_driver, rv_sim) {
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <- session$ns
  max_individuals <- get_golem_options("maxIndividuals")

  moduleServer(id, function(input, output, session) {

    submit_clicked <- reactive({
      input$simulateCohort
    })

    mod_loader_server(
      ns("loader_simulate"),
      session,
      "simulateCohort",
      submit_clicked,
      "Simulation in progress",
      "Please wait while the simulation is ongoing...",
      input$arraySizeInput
    )

    mod_show_history_button_server(
      "SimulateHistorySidebar",
      "sim",
      "SimulateHistorySidebar",
      db_conn
    )

    iv <- InputValidator$new()
    iv$add_rule("arraySizeInput", sv_between(0, max_individuals))
    iv$enable()

    data <- data.frame(
      ontologies = c(
        "diseases",
        "exposures",
        "phenotypicFeatures",
        "procedures",
        "treatments"
      ),
      count = c(3, 3, 3, 3, 3),
      max_pool_size = c(5, 5, 5, 5, 5)
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

    simulationOutputFolder <- get_golem_options("simulationOutputFolder")

    observe({
      req(rv_sim$simulationId)

      path <- paste0(simulationOutputFolder, rv_sim$simulationId)
      bffOutputFn <- paste0(path, ".bff.json")
      pxfOutputFn <- paste0(path, ".pxf.json")

      print("bffOutputFn")
      print(bffOutputFn)

      output$bffDl <- outputDownloadHandler(bffOutputFn, "phenoRankerSim.bff")
      output$pxfDl <- outputDownloadHandler(pxfOutputFn, "phenoRankerSim.pxf")
      output$allDl <- outputDownloadHandler(
        list(bffOutputFn, pxfOutputFn),
        list("phenoRankerSim.bff", "phenoRankerSim.pxf"),
        output_name = "phenoRankerSim.zip",
        zip_download = TRUE
      )
    })

    observeEvent(input$ontologiesFile, {
      req(input$ontologiesFile)

      allowed_types <- c("yaml", "yml")
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
        updateAceEditor(session, "yamlEditor_procedures", value = "")
        updateAceEditor(session, "yamlEditor_expos", value = "")
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

      updateAceEditor(
        session,
        "yamlEditor_procedures",
        value = as.yaml(yaml_data$procedures)
      )

      updateAceEditor(
        session,
        "yamlEditor_expos",
        value = as.yaml(yaml_data$exposures)
      )
    })

    editors <- c(
      "yamlEditor_diseases",
      "yamlEditor_phenos",
      "yamlEditor_treatments",
      "yamlEditor_procedures",
      "yamlEditor_expos"
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

    observeEvent(input$inputs, {
      rv_sim$dtInputs <- input$inputs
    })

    observeEvent(input$simulateBtnClicked, {
      req(iv$is_valid())
      req(rv_sim$dtInputs)
      simSettings <- c(rv_sim$dtInputs, input$arraySizeInput)

      simulationId <- writeYAMLDataToFile(
        input$yamlEditor_diseases,
        input$yamlEditor_expos,
        input$yamlEditor_phenos,
        input$yamlEditor_procedures,
        input$yamlEditor_treatments
      )

      session$sendCustomMessage(
        type = "changeURL",
        message = list(
          mode = "sim",
          id = simulationId
        )
      )

      output$simulationId <- renderText(paste0("RUN ID: ", simulationId))

      selectedOutputFormats <- input$checkboxes


      # create the external ontology setting string

      # maybe better get it from a config file
      flags <- c(
        "diseases",
        "exposures",
        "phenotypicFeatures",
        "procedures",
        "treatments"
      )

      external_ontologies_settings <- unlist(
        lapply(
          flags,
          function(flag) {
            c(
              paste0("-", flag),
              paste0("-max-", flag, "-pool")
            )
          }
        )
      )

      indices <- seq(1, length(external_ontologies_settings))

      # Loop through the settings and add them
      ext_onts_settings_mapping <- list()
      ext_onts_settings_string <- ""
      for (i in seq(1, length(indices), by = 2)) {
        ontology <- external_ontologies_settings[i]
        max_pool <- external_ontologies_settings[i + 1]

        ontology_count <- simSettings[indices[i]]
        max_pool_size <- simSettings[indices[i + 1]]

        ext_onts_settings_string <- paste0(
          ext_onts_settings_string, ontology, " ", ontology_count, " ",
          max_pool, " ", max_pool_size, " "
        )

        ext_onts_settings_mapping[[ontology]] <- ontology_count
        ext_onts_settings_mapping[[max_pool]] <- max_pool_size
      }

      number_of_individuals <- simSettings[11]

      print("ext_onts_settings_string")
      print(ext_onts_settings_string)

      lapply(selectedOutputFormats, function(option) {
        if (option == "BFF") {
          rv_sim$simResult_bff <- simulate_data(
            "bff",
            simulationId,
            ext_onts_settings_string,
            number_of_individuals
          )
        } else if (option == "PXF") {
          rv_sim$simResult_pxf <- simulate_data(
            "pxf",
            simulationId,
            ext_onts_settings_string,
            number_of_individuals
          )
        }
      })

      print("input$arraySizeInput")
      print(input$arraySizeInput)
      mod_json_viewer_server(
        ns("json_viewer_sim_mode"),
        selectedOutputFormats,
        rv_sim$simResult_bff,
        rv_sim$simResult_pxf,
        input$arraySizeInput
      )

      rv_sim$simulationId <- simulationId

      settings <- list(
        outputFormats = tolower(selectedOutputFormats),
        externalOntologies = ext_onts_settings_mapping,
        numberOfIndividuals = number_of_individuals
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

      label <- paste0("Simulation ID: ", simulationId)
      print("label")
      print(label)

      print("db_driver")
      print(db_driver)

      store_job_in_db(
        simulationId,
        userId,
        "sim",
        label,
        settings,
        db_conn
      )
      click("SimulateHistorySidebar-btn_show_history")
    })
  })
}
