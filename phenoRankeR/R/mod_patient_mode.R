#' Patient Mode Module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom gridlayout grid_container grid_card grid_place new_gridlayout
#' @importFrom shiny NS actionButton
#' @importFrom shinyjs click reset
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom jsonlite toJSON
#' @importFrom jqr jq
#' @importFrom utils write.csv

mod_patient_mode_layout <- c(
  "         550px   1fr           40px",
  "35px     btn     phenoBlast    btn_show_patient_history",
  "320px    opts    phenoBlast    btn_show_patient_history",
  "1fr      opts    phenoHeadsUp  btn_show_patient_history",
  "1px      version version       version                 "
)

patient_opts_layout <- c(
  "         1fr       ",
  "380px    rankerInput",
  "350px    configYamls",
  "350px    variables  "
)

patient_mode_layout <- new_gridlayout(
  c(
    "btn      phenoBlast      btn_show_patient_history",
    "opts     phenoBlast      btn_show_patient_history",
    "opts     phenoHeadsUp    btn_show_patient_history",
    "version  version         version"
  ),
  col_sizes = c("550px", "1fr", "40px"),
  alternate_layouts = list(
    layout = c(
      "         300px   1fr           40px",
      "35px     btn     phenoBlast    btn_show_patient_history",
      "320px    opts    phenoBlast    btn_show_patient_history",
      "1fr      opts    phenoHeadsUp  btn_show_patient_history",
      "1px      version version       version                 "
    ),
    width_bounds = c(max = 1110)
  )
)


mod_patient_mode_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")

  grid_container(
    layout = patient_mode_layout,
    gap_size = "5px",
    grid_place(
      area = "btn",
      actionButton(
        ns("rankPatient"),
        "Rank",
        class = "btn btn-primary"
      )
    ),
    grid_card(
      area = "opts",
      card_header("Options"),
      # TODO
      # missed CSS class to remove the top padding
      grid_container(
        layout = patient_opts_layout,
        gap_size = "0px",
        grid_card(
          area = "rankerInput",
          card_body(
            style = "padding-top: 5px; padding-left: 16px;",
            tabsetPanel(
              id = ns("patientRankerTabsetPanel"),
              selected = "Reference(s)",
              type = "pills",
              tabPanel(
                title = "Reference(s)",
                card_body(
                  style = "padding-top: 0px; padding-left: 16px;",
                  span("Select your data source:"),
                  tabsetPanel(
                    id = ns("patientRankerReferenceTabsetPanel"),
                    selected = "Upload",
                    tabPanel(
                      title = "Upload",
                      fileInput(
                        ns("referenceFiles"),
                        "Upload a JSON or multiple as reference cohort(s)",
                        multiple = TRUE,
                        accept = c(
                          ".json"
                        )
                      )
                      # TODO
                      # add css to remove the bottom margin
                    ),
                    # TODO as soon there is file uploaded
                    # select simulated data should be disabled
                    tabPanel(
                      title = "Beacon API",
                      selectInput(
                        ns("patient_beacon_api_reference"),
                        "Select cohort(s)",
                        choices = NULL,
                        multiple = TRUE
                      )
                    ),
                    tabPanel(
                      title = "Retrieved Examples",
                      selectInput(
                        ns("patient_example_reference"),
                        "Select cohort(s)",
                        choices = NULL,
                        multiple = TRUE
                      )
                    ),
                    tabPanel(
                      title = "Simulation",
                      grid_container(
                        layout = c(
                          "     1fr      1fr   ",
                          "1fr  dropdown radio "
                        ),
                        grid_place(
                          area = "dropdown",
                          selectInput(
                            ns("patient_sim_reference"),
                            "Select simulated cohort(s)",
                            choices = NULL,
                            multiple = TRUE
                          )
                        ),
                        grid_place(
                          area = "radio",
                          uiOutput(ns("simulatedRefsInputFormats"))
                        )
                      )
                    ),
                    tabPanel(
                      title = "Conversion",
                      card_body(
                        selectInput(
                          ns("patient_conv_reference"),
                          "Select a converted cohort",
                          choices = NULL,
                          multiple = TRUE
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                title = "Target",
                card_body(
                  style = "padding-top: 0px; padding-left: 16px;",
                  span("Select your data source:"),
                  tabsetPanel(
                    id = ns("patientRankerTargetTabsetPanel"),
                    selected = "Upload",
                    tabPanel(
                      title = "Upload",
                      fileInput(
                        ns("targetFile"),
                        "Upload a BFF/PXF file of an individual",
                        multiple = FALSE,
                        accept = c(
                          ".json"
                        )
                      )
                    ),
                    # insert should be only in yaml format
                    # TODO
                    # add comment in the first line of the editor
                    # to explain that the format should be yaml
                    tabPanel(
                      title = "Insert",
                      aceEditor(
                        "yamlEditor",
                        value = "",
                        mode = "yaml",
                        theme = "github",
                        height = "125px"
                      )
                    ),
                    tabPanel(
                      title = "Beacon API",
                      selectInput(
                        ns("patient_beacon_api_target"),
                        HTML(
                          "
                            Select individual<br>
                            <small>(the first individual will be used)</small>
                          "
                        ),
                        choices = NULL
                      )
                    ),
                    tabPanel(
                      title = "Retrieved Examples",
                      selectInput(
                        ns("patient_example_target"),
                        HTML(
                          "
                            Select individual<br>
                            <small>(the first individual will be used)</small>
                          "
                        ),
                        choices = NULL
                      )
                    ),
                    tabPanel(
                      title = "Simulation",
                      selectInput(
                        ns("patient_sim_target"),
                        HTML(
                          "
                            Select a simulated BFF/PXF<br>
                            <small>(the first individual will be used)</small>
                          "
                        ),
                        choices = NULL
                      )
                    ),
                    tabPanel(
                      title = "Conversion",
                      selectInput(
                        ns("patient_conv_target"),
                        HTML(
                          "
                            Select a converted target<br>
                            <small>(the first individual will be used)</small>
                          "
                        ),
                        choices = NULL
                      )
                    )
                  )
                )
              )
            ),
            span("Set individuals id prefix for each cohort"),
            aceEditor(
              ns("yamlEditorIdPrefixes"),
              value = "",
              mode = "yaml",
              theme = "github",
              height = "60px"
            ),
            verbatimTextOutput(ns("idPrefixesYamlErrorOutput"))
          )
        ),
        grid_card(
          area = "configYamls",
          tabsetPanel(
            selected = "Weights",
            tabPanel(
              title = "Weights",
              card_body(
                fileInput(
                  ns("weightsFile"),
                  "Upload a weights yaml file",
                  multiple = FALSE,
                  accept = c(
                    ".yaml"
                  )
                ),
                p("or add the weights below:"),
                aceEditor(
                  ns("yamlEditor_weights"),
                  value = weights_defaults,
                  mode = "yaml",
                  theme = "github",
                  height = "60px"
                ),
                verbatimTextOutput(ns("weightsYamlErrorOutput"))
              )
            ),
            tabPanel(
              title = "Extra Config",
              card_body(
                fileInput(
                  ns("configFile"),
                  "Upload a config yaml file",
                  multiple = FALSE,
                  accept = c(
                    ".yaml"
                  )
                ),
                p("or add the config below:"),
                aceEditor(
                  ns("yamlEditor_config"),
                  value = "",
                  mode = "yaml",
                  theme = "github",
                  height = "60px"
                ),
                verbatimTextOutput(ns("configYamlErrorOutput"))
              )
            )
          )
        ),
        grid_place(
          area = "variables",
          mod_dnd_ui(ns("patient_incl_excl_list"))
        )
      )
    ),
    grid_card(
      area = "phenoBlast",
      card_header("Target vs all reference individuals"),
      full_screen = TRUE,
      card_body(
        # TODO
        # add the RUN ID to the Cohort comparison view as well
        # the run id will then be used to check if the run
        # was weighted or not
        # maybe that information is stored in the log file?
        verbatimTextOutput(ns("phenoBlastRunId")),
        tabsetPanel(
          id = ns("patientRankingResults"),
          selected = "Ranking",
          tabPanel(
            title = "Ranking",
            mod_table_phenoRanking_ui(ns("phenoRankingTable"))
          ),
          tabPanel(
            title = "Binary representation",
            mod_table_phenoBlast_ui(ns("binaryRepresentationTable"))
          ),
          tabPanel(
            title = "Hamming Distances Heatmap",
            card_body(
              # p("Use the first row of the heatmap to select a reference individual"),
              htmlOutput(ns("patient_heatmap")),
            )
          ),
          tabPanel(
            title = "Multi Dimensional Scaling Scatter Plot",
            mod_plot_mds_ui(ns("mds_scatter"))
          )
        )
      )
    ),
    grid_card(
      area = "phenoHeadsUp",
      card_header("Target vs a selected reference individual"),
      full_screen = TRUE,
      mod_table_phenoHeadsUp_ui(ns("phenoHeadsUpTable"))
    ),
    grid_place(
      area = "btn_show_patient_history",
      mod_show_history_button_ui(ns("PatientHistorySidebar"))
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

# TODO below is the same for patient and cohort mode
weights_defaults <- paste(
  readLines("inst/extdata/defaults/weights_defaults.yaml"),
  collapse = "\n"
)

incl_excl_criteria <- fromJSON(
  readLines(
    "inst/extdata/config/incl_excl_criteria.json"
  )
)

mod_patient_mode_server <- function(
    id,
    session,
    db_conn,
    rv_patient,
    rv_beacon_api,
    rv_input_examples,
    rv_sim,
    rv_conversion,
    rv_general) {
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <- session$ns
  moduleServer(id, function(input, output, session) {
    submit_clicked <- reactive({
      input$rankPatient
    })

    mod_loader_server(
      "loader_patient_mode",
      session,
      "rankPatient",
      submit_clicked,
      "Patient Mode",
      "Please wait while the ranking is ongoing...",
      NULL
    )

    mod_show_history_button_server(
      "PatientHistorySidebar",
      "patient",
      "PatientHistorySidebar",
      db_conn,
      rv_general$user_email
    )

    mod_dnd_server(
      ns("patient_incl_excl_list"),
      rv_patient
    )

    mod_table_phenoBlast_server("binaryRepresentationTable", rv_general)
    mod_table_phenoRanking_server("phenoRankingTable", rv_general)
    mod_table_phenoHeadsUp_server("phenoHeadsUpTable", rv_general)
    mod_plot_mds_server("mds_scatter", rv_general)

    output$patient_heatmap <- renderUI({
      p("Click on Rank to generate the heatmap")
    })

    handleFileUpload <- function(input_id, rv, rv_general, targetDir) {
      print("handleFileUpload")

      # rank_input_dir <- get_golem_options("rankInputFolder")

      rank_input_dir <- paste0(
        rv_general$user_dirs$uploads,
        "/rankInput/"
      )

      file <- input[[input_id]]
      print("file")
      print(file)

      if (is.null(file)) {
        return(NULL)
      }

      allowed_types <- c("json")

      # add to an extra file called validators.R
      if (!(get_file_ext(file$name) %in% allowed_types)) {
        showNotification("Invalid file type!", type = "error")
        reset(input_id)
        return(NULL)
      }

      # add to an extra file called validators.R
      if (input_id == "targetFile") {
        # check if the target file is a json obj
        jsonObj <- fromJSON(
          input$targetFile$datapath,
          simplifyDataFrame = FALSE
        )

        if (is.null(names(jsonObj))) {
          showNotification(
            "Please upload a single json object and not an array of json objects",
            type = "error"
          )
          reset("targetFile")
          return(NULL)
        }
      }

      # TODO
      # put this in a extra function
      json_data <- fromJSON(readLines(file$datapath))
      rv$inputFormat <- "bff"
      if ("subject" %in% names(json_data)) {
        rv$inputFormat <- "pxf"
      }
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      fn <- paste0(timestamp, ".", rv$inputFormat, ".json")

      targetDir <- paste0(rank_input_dir, targetDir)
      file_path <- file.path(targetDir, fn)
      file.copy(file$datapath, file_path)

      if (grepl("references", targetDir)) {
        rv$uploadedReferenceFile <- fn
      } else {
        rv$uploadedTargetFile <- fn
      }
      return(TRUE)
    }

    observeEvent(rv_patient$id, {
      if (is.null(rv_patient$id)) {
        return()
      }
      mod_table_phenoHeadsUp_server(
        "phenoHeadsUpTable",
        rv_general,
        rv_patient = rv_patient
      )
    })

    observeEvent(input$patientRankerReferenceTabsetPanel, {
      print(input$patientRankerReferenceTabsetPanel)

      if (input$patientRankerReferenceTabsetPanel == "Beacon API") {
        observeTabChangeToBeaconApiData(
          input,
          session,
          db_conn,
          "patientRankerReferenceTabsetPanel",
          "patient_beacon_api_reference",
          rv_general$user_email
        )
      }

      if (input$patientRankerReferenceTabsetPanel == "Retrieved Examples") {
        req(rv_patient$inputFormat)
        rv_patient$inputFormat <- "pxf"

        observeTabChangeToExampleData(
          input,
          session,
          db_conn,
          "patientRankerReferenceTabsetPanel",
          "patient_example_reference",
          rv_general$user_email
        )
      }

      if (input$patientRankerReferenceTabsetPanel == "Simulation") {
        observeTabChangeToSimulateData(
          input,
          session,
          db_conn,
          "patientRankerReferenceTabsetPanel",
          "patient_sim_reference",
          rv_general$user_email
        )
      }

      if (input$patientRankerReferenceTabsetPanel == "Conversion") {
        observeTabChangeToConvertedData(
          input,
          session,
          db_conn,
          "patientRankerReferenceTabsetPanel",
          "patient_conv_reference",
          rv_general$user_email
        )
      }
    })

    observeEvent(input$patientRankerTargetTabsetPanel, {
      print("input$patientRankerTargetTabsetPanel")
      print(input$patientRankerTargetTabsetPanel)

      if (input$patientRankerTargetTabsetPanel == "Beacon API") {
        req(rv_patient$inputFormat)

        if (rv_patient$inputFormat == "pxf") {
          showNotification(
            "Beacon API data is only available for BFF format",
            type = "error"
          )
          updateTabsetPanel(
            session,
            "patientRankerTargetTabsetPanel",
            "Simulation"
          )
          return()
        }

        observeTabChangeToBeaconApiData(
          input,
          session,
          db_conn,
          "patientRankerTargetTabsetPanel",
          "patient_beacon_api_target",
          rv_general$user_email
        )
      }

      if (input$patientRankerTargetTabsetPanel == "Retrieved Examples") {
        req(rv_patient$inputFormat)
        if (rv_patient$inputFormat == "bff") {
          showNotification(
            "Example data is only available for PXF format",
            type = "error"
          )
          updateTabsetPanel(
            session,
            "patientRankerTargetTabsetPanel",
            "Simulation"
          )
          return()
        }

        observeTabChangeToExampleData(
          input,
          session,
          db_conn,
          "patientRankerTargetTabsetPanel",
          "patient_example_target",
          rv_general$user_email
        )
      }

      if (input$patientRankerTargetTabsetPanel == "Simulation") {
        observeTabChangeToSimulateData(
          input,
          session,
          db_conn,
          "patientRankerTargetTabsetPanel",
          "patient_sim_target",
          rv_general$user_email
        )
      }

      if (input$patientRankerTargetTabsetPanel == "Conversion") {
        observeTabChangeToConvertedData(
          input,
          session,
          db_conn,
          "patientRankerTargetTabsetPanel",
          "patient_conv_target",
          rv_general$user_email
        )
      }
    })

    observeTabChangeEvent(
      input,
      session,
      "patientRankerTabsetPanel",
      "Target",
      function() {
        is.null(rv_patient$uploadedReferenceFile) &&
          rv_patient$useBeaconReference == FALSE &&
          rv_patient$useExampleReference == FALSE &&
          rv_patient$useSimulatedReference == FALSE &&
          rv_patient$useConvertedReference == FALSE
      },
      "Please upload/select the reference(s) first",
      "Reference(s)"
    )

    # TODO
    # put all these functions
    # in a module called
    # mod_patients_options_sidebar.R

    # TODO
    # merge input$referenceFiles and input$cohortModeFiles
    observeEvent(input$referenceFiles, {
      req(input$referenceFiles)

      # rank_input_dir <- get_golem_options("patientRankInputRefsFolder")
      rank_input_dir <- rv_general$user_dirs$uploads$refs

      allowed_types <- c("json")

      input_format <- NULL
      references <- list()
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

      str_references <- ""
      mapping_df <- data.frame(
        original_fn = character(),
        new_fn = character(),
        id_prefixes = character(),
        simulatedData = logical(),
        file_info = character(),
        stringsAsFactors = FALSE
      )

      total_individual_counts <- 0
      for (i in 1:nrow(input$referenceFiles)) {
        uploaded_file <- input$referenceFiles[i, ]

        file_con <- file(uploaded_file$datapath, "r")
        individual_counts <- as.numeric(jq(file_con, "length"))
        close(file_con)

        total_individual_counts <- total_individual_counts + individual_counts
        if (total_individual_counts > 1000) {
          showNotification(
            "The maximum number of individuals is 1000",
            type = "error"
          )
          reset("referenceFiles")
          return()
        }

        if (!(get_file_ext(uploaded_file$name) %in% allowed_types)) {
          showNotification("Invalid file type!", type = "error")
          reset("referenceFiles")
          return()
        }

        json_data <- fromJSON(readLines(uploaded_file$datapath))
        rv_patient$inputFormat <- "bff"
        if ("subject" %in% names(json_data)) {
          rv_patient$inputFormat <- "pxf"
        }

        if (is.null(input_format)) {
          input_format <- rv_patient$inputFormat
        } else if (input_format != rv_patient$inputFormat) {
          showNotification(
            "All reference files must be of the same format!",
            type = "error"
          )
          return()
        }

        str_references <- paste0(
          str_references,
          uploaded_file$name,
          ":R",
          i,
          "\n"
        )

        fn <- paste0(
          timestamp,
          "_",
          i,
          ".",
          rv_patient$inputFormat,
          ".json"
        )
        references[[i]] <- fn


        file_path <- file.path(
          rank_input_dir,
          fn
        )

        file.copy(
          uploaded_file$datapath,
          file_path
        )

        row <- data.frame(
          original_fn = uploaded_file$name,
          new_fn = normalizePath(paste0(rank_input_dir, "/", fn)),
          id_prefixes = paste0("R", i),
          simulatedData = FALSE,
          file_info = "Reference",
          stringsAsFactors = FALSE
        )
        mapping_df <- rbind(mapping_df, row)
      }

      rv_patient$uploadedReferenceFile <- references

      updateAceEditor(
        session,
        "yamlEditorIdPrefixes",
        value = str_references
      )
      rv_patient$mappingDf <- mapping_df
      rv_patient$useSimulatedReference <- FALSE
      rv_patient$useConvertedReference <- FALSE
    })

    observeEvent(input$targetFile, {
      req(input$targetFile)
      req(rv_patient$mappingDf)

      print("observeEvent(input$targetFile")
      print("rv_patient$mappingDf")
      print(rv_patient$mappingDf)

      mapping_df <- rv_patient$mappingDf

      res <- handleFileUpload(
        "targetFile",
        rv_patient,
        rv_general,
        "patientMode/targets"
      )

      if (is.null(res)) {
        return(NULL)
      }

      row <- data.frame(
        original_fn = input$targetFile$name,
        new_fn = rv_patient$uploadedTargetFile,
        id_prefixes = "T",
        simulatedData = FALSE,
        file_info = "Target",
        stringsAsFactors = FALSE
      )

      print("row")
      print(row)

      mapping_df <- subset(mapping_df, mapping_df$file_info != "Target")
      print("mapping_df after subset")
      print(mapping_df)

      mapping_df <- rbind(mapping_df, row)
      print("mapping_df after rbind")
      print(mapping_df)


      # put this into a general function
      editor_val <- ""
      for (i in 1:nrow(mapping_df)) {
        editor_val <- paste0(
          editor_val,
          mapping_df$original_fn[i],
          ":",
          mapping_df$id_prefixes[i],
          "\n"
        )
      }
      rv_patient$mappingDf <- mapping_df

      updateAceEditor(
        session,
        "yamlEditorIdPrefixes",
        value = editor_val
      )
      rv_patient$useSimulatedTarget
      rv_patient$useConvertedTarget
    })

    observeEvent(input$weightsFile, {
      req(input$weightsFile)

      allowed_types <- c("yaml")
      # file_ext <- file_ext(input$weightsFile$name)

      if (!(get_file_ext(input$weightsFile$name) %in% allowed_types)) {
        showNotification("Invalid file type!", type = "error")
        reset("weightsFile")
        return()
      }

      file_data <- paste(
        readLines(
          input$weightsFile$datapath
        ),
        collapse = "\n"
      )

      yamlValid <- validateYAML(file_data)
      if (yamlValid != "YAML is valid") {
        showNotification(yamlValid, type = "error")
        output$weightsYamlErrorOutput <- renderText(yamlValid)
        updateAceEditor(session, "yamlEditor_weights", value = "")
        return()
      }
      output$weightsYamlErrorOutput <- renderText(yamlValid)

      updateAceEditor(
        session,
        "yamlEditor_weights",
        value = as.yaml(yaml.load(file_data))
      )
    })

    observeEvent(input$yamlEditor_weights, {
      result <- validateYAML(input$yamlEditor_weights)
      output$weightsYamlErrorOutput <- renderText(result)
    })

    # it is exactly the same as the weights file upload
    # so it should be put in a general function
    observeEvent(input$configFile, {
      req(input$configFile)

      allowed_types <- c("yaml")
      if (!(get_file_ext(input$configFile$name) %in% allowed_types)) {
        showNotification("Invalid file type!", type = "error")
        reset("configFile")
        return()
      }

      file_data <- paste(
        readLines(
          input$configFile$datapath
        ),
        collapse = "\n"
      )

      yamlValid <- validateYAML(file_data)
      if (yamlValid != "YAML is valid") {
        showNotification(yamlValid, type = "error")
        output$configYamlErrorOutput <- renderText(yamlValid)
        updateAceEditor(session, "yamlEditor_config", value = "")
        return()
      }
      output$configYamlErrorOutput <- renderText(yamlValid)

      updateAceEditor(
        session,
        "yamlEditor_config",
        value = as.yaml(yaml.load(file_data))
      )
    })

    observeEvent(input$yamlEditor_config, {
      result <- validateYAML(input$yamlEditor_config)
      output$configYamlErrorOutput <- renderText(result)

      print("input$yamlEditor_config")
      data <- yaml.load(input$yamlEditor_config)
      print(data)

      rv_patient$allowedTerms <- data$allowed_terms
    })

    # set_input_paths <- function(rv, rv_beacon_api, rv_input_examples,
    #   rv_sim, rv_conversion, rv_general, mode) {
    #   print("set_input_paths")
    #   print("rv_input_examples$retrievalId")
    #   print(rv_input_examples$retrievalId)

    #   beaconApiDataPath <- paste0(
    #     rv_general$user_dirs$output$beacon,
    #     "/",
    #     rv_beacon_api$queryId,
    #     ".bff.json"
    #   )

    #   exampleDataPath <- paste0(
    #     rv_general$user_dirs$output$example,
    #     "/",
    #     rv_input_examples$retrievalId,
    #     ".pxf.json"
    #   )

    #   print("exampleDataPath")
    #   print(exampleDataPath)

    #   simDataPath <- paste0(
    #     rv_general$user_dirs$output$sim,
    #     "/",
    #     rv_sim$simulationId,
    #     ".",
    #     rv$inputFormat,
    #     ".json"
    #   )

    #   convDataPath <- paste0(
    #     rv_general$user_dirs$output$conv,
    #     "/",
    #     rv_conversion$id,
    #     "/",
    #     rv_conversion$id,
    #     ".json"
    #   )

    #   upload_dir <- paste0(
    #     rv_general$user_dirs$uploads$rankInput,
    #     "/",
    #     mode,
    #     "/"
    #   )

    #   file_paths <- c(
    #     "reference_file_path" = paste0(
    #       upload_dir,
    #       "references/",
    #       rv$uploadedReferenceFile
    #     ),
    #     "target_file_path" = paste0(
    #       upload_dir,
    #       "targets/",
    #       rv$uploadedTargetFile
    #     )
    #   )
    #   print(file_paths)

    #   # TODO
    #   # store the orgin of the referece/target file

    #   # origin_job_ids_references
    #   # origin_job_id_target

    #   origin_job_ids_references <- NULL
    #   origin_job_id_target <- NULL
    #   if (mode == "patientMode" && rv$useBeaconReference) {
    #     file_paths["reference_file_path"] <- beaconApiDataPath
    #     origin_job_ids_references <- rv_beacon_api$queryId
    #   }

    #   if (mode == "patientMode" && rv$useExampleReference) {
    #     file_paths["reference_file_path"] <- exampleDataPath
    #     origin_job_ids_references <- rv_input_examples$retrievalId
    #   }

    #   if (mode == "patientMode" && rv$useSimulatedReference) {
    #     file_paths["reference_file_path"] <- simDataPath
    #     origin_job_ids_references <- rv_sim$simulationId
    #   }

    #   if (mode == "patientMode" && rv$useConvertedReference) {
    #     file_paths["reference_file_path"] <- convDataPath
    #     origin_job_ids_references <- rv_conversion$id
    #   }

    #   if (mode == "patientMode" && rv$useBeaconTarget) {
    #     file_paths["target_file_path"] <- generate_target_based_on_beacon_data(
    #       rv,
    #       rv_beacon_api,
    #       rv_general,
    #       beaconApiDataPath
    #     )
    #     origin_job_id_target <- rv_beacon_api$queryId
    #   }

    #   if (mode == "patientMode" && rv$useExampleTarget) {
    #     file_paths["target_file_path"] <- generate_target_based_on_example_data(
    #       rv,
    #       rv_input_examples,
    #       rv_general,
    #       exampleDataPath
    #     )
    #     origin_job_id_target <- rv_input_examples$retrievalId
    #   }

    #   if (mode == "patientMode" && rv$useSimulatedTarget) {
    #     file_paths["target_file_path"] <- generate_target_based_on_simulated_data(
    #       rv,
    #       rv_sim,
    #       rv_general,
    #       simDataPath
    #     )
    #     origin_job_id_target <- rv_sim$simulationId
    #   }

    #   if (mode == "patientMode" && rv$useConvertedTarget) {
    #     file_paths["target_file_path"] <- generate_target_based_on_converted_data(
    #       rv,
    #       rv_conversion,
    #       rv_general,
    #       convDataPath
    #     )
    #     origin_job_id_target <- rv_conversion$id
    #   }

    #   file_paths <- lapply(file_paths, function(path) {
    #     info <- file.info(path)
    #     if (!info$isdir & !is.na(info$isdir)) {
    #       normalizePath(path)
    #     } else {
    #       print("NULL")
    #       print(path)
    #       NULL
    #     }
    #   })

    #   # TODO
    #   # Figure out
    #   # this function seem to be only called
    #   # if one reference is used

    #   print("origin_job_ids_references")
    #   print(origin_job_ids_references)

    #   print("origin_job_id_target")
    #   print(origin_job_id_target)

    #   print("file_paths")
    #   print(file_paths)
    #   return(file_paths)
    # }

    observeEvent(input$patientRankingBtnClicked, {
      print("rankPatient")

      updateTabsetPanel(
        session,
        "patientRankingResults",
        "Hamming Distances Heatmap"
      )

      output$patient_heatmap <- renderUI({
        p("Generating heatmap...")
      })

      # TODO
      # when using simulated data
      # the yaml to change the id prefixes
      # should be changed as well

      # TODO
      # it s not working when using simulated data
      # for the reference and real data for the target

      # TODO
      # it should fail when the user tries to rank
      # while include/exclude are both filled

      # Explain to the user that include/exclude
      # are mutually exclusive

      # print("before set_input_paths")
      # paths <- set_input_paths(
      #   rv_patient,
      #   rv_beacon_api,
      #   rv_input_examples,
      #   rv_sim,
      #   rv_conversion,
      #   rv_general,
      #   "patientMode"
      # )
      # print("after set_input_paths")
      # print("paths")
      # print(paths)

      # For what "reference_file_path1" is used?
      # if ("reference_file_path1" %in% names(paths)) {
      #   inputReferenceFilePath <- paths["reference_file_path1"]
      # } else {
      #   inputReferenceFilePath <- paths["reference_file_path"]
      # }
      # inputTargetFilePath <- paths["target_file_path"]

      # TODO
      # put it in a extra files called errorHandlers.R
      # if (is.null(inputReferenceFilePath[[1]]) || is.null(inputTargetFilePath[[1]])) {
      #   print("inputReferenceFilePath[[1]]")
      #   print(inputReferenceFilePath[[1]])

      #   print("inputTargetFilePath[[1]]")
      #   print(inputTargetFilePath[[1]])

      #   showNotification(
      #     "Please upload or select a example/simulated reference and target file!",
      #     type = "error"
      #   )
      #   return()
      # }

      print("rv_patient$mappingDf")
      print(rv_patient$mappingDf)

      if (is.null(rv_patient$mappingDf)) {
        showNotification(
          "Please upload or select a example/simulated reference and target file!",
          type = "error"
        )
        return()

        # TODO
        # loading spinner should not be triggered
      }

      ref_files <- rv_patient$mappingDf$new_fn[1:nrow((rv_patient$mappingDf)) - 1]
      print("ref_files")
      print(ref_files)

      # get the target file of mappingDf filtered by file_info == "Target"
      inputTargetFilePath <- rv_patient$mappingDf$new_fn[rv_patient$mappingDf$file_info == "Target"]

      if (is.null(inputTargetFilePath) || is.null(ref_files)) {
        showNotification(
          "Please upload or select a example/simulated reference and target file!",
          type = "error"
        )
        return()
      }

      # remove the filename from the path
      inputTargetFileDir <- dirname(inputTargetFilePath)
      print("inputTargetFileDir")
      print(inputTargetFileDir)

      input_format <- "json"
      # check if the basename does not equal to targets
      if (basename(inputTargetFileDir) != "targets") {
        print("inputTargetFilePath")
        print(inputTargetFilePath)

        # generate the target file
        jsonArray <- fromJSON(
          inputTargetFilePath,
          simplifyDataFrame = FALSE,
        )

        jsonObj <- jsonArray[[1]]
        print("jsonObj")
        print(jsonObj)

        base_fn <- basename(inputTargetFilePath)

        # insert .target after the first dot
        # so the filename will be like this "runID.target.bff.json"
        new_base_fn <- sub(
          "(\\.[^\\.]+)$", ".target\\1",
          base_fn
        )

        # Extract bff or pxf from the filename
        match <- regmatches(base_fn, regexpr("(bff|pxf)", base_fn))

        # Store it in a variable so it later can be saved in the database
        if (length(match) > 0) {
          input_format <- paste0(match, ".json")
        }

        print("input_format")
        print(input_format)

        targetFilePath <- paste0(
          inputTargetFileDir,
          "/",
          new_base_fn
        )

        print("targetFilePath")
        print(targetFilePath)

        file.create(targetFilePath)

        fileConn <- file(
          targetFilePath,
          open = "w"
        )

        writeLines(
          toJSON(
            jsonObj,
            pretty = TRUE,
            auto_unbox = TRUE
          ),
          con = fileConn
        )
        close(fileConn)
      }

      # generate the target file
      # jsonArray <- fromJSON(
      #   normalizePath(
      #     file.path(
      #       inputTargetFilePath
      #     )
      #   ),
      #   simplifyDataFrame = FALSE,
      # )
      # jsonObj <- jsonArray[[1]]

      # targetFilePath <- paste0(
      #   inputTargetFileDir,
      #   "/",
      #   rv_sim$simulationId,
      #   ".target.",
      #   rv$inputFormat,
      #   ".json"
      # )
      # file.create(targetFilePath)

      # fileConn <- file(
      #   targetFilePath,
      #   open = "w"
      # )
      # writeLines(
      #   toJSON(
      #     jsonObj,
      #     pretty = TRUE,
      #     auto_unbox = TRUE
      #   ),
      #   con = fileConn
      # )
      # close(fileConn)

      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      rv_patient$runId <- timestamp

      session$sendCustomMessage(
        type = "changeURL",
        message = list(mode = "patient", id = timestamp)
      )

      runId <- paste0(
        "RUN ID: ",
        timestamp
      )

      output$phenoBlastRunId <- renderText(runId)

      outDir <- paste0(
        paste0(
          rv_general$user_dirs$output$pats_ranked,
          "/",
          timestamp,
          "/"
        )
      )

      dir.create(
        outDir,
        recursive = TRUE
      )

      write.csv(
        rv_patient$mappingDf,
        file = paste0(
          rv_general$user_dirs$output$pats_ranked,
          "/",
          rv_patient$runId,
          "/",
          rv_patient$runId,
          "_mapping.csv"
        ),
        row.names = FALSE
      )

      weights_file_path <- NULL
      if (input$yamlEditor_weights != "") {
        fn <- paste0(
          timestamp,
          ".yaml"
        )
        weights_file_path <- file.path(
          rv_general$user_dirs$uploads$weights,
          fn
        )

        writeLines(
          input$yamlEditor_weights,
          weights_file_path
        )
      }

      extra_config_file_path <- NULL
      if (input$yamlEditor_config != "") {
        extra_config_file_path <- file.path(
          rv_general$user_dirs$uploads$config,
          paste0(timestamp, "_config.yaml")
        )

        print("extra_config_file_path")
        print(extra_config_file_path)

        print("input$yamlEditor_config")
        print(input$yamlEditor_config)

        writeLines(
          input$yamlEditor_config,
          extra_config_file_path
        )
      }
      print("weights_file_path")
      print(weights_file_path)

      print("rv_patient$mappingDf")
      print(rv_patient$mappingDf)

      # ref_files <- rv_patient$mappingDf$new_fn[1:nrow((rv_patient$mappingDf)) - 1]

      # # get the target file of mappingDf filtered by file_info == "Target"
      # inputTargetFilePath <- rv_patient$mappingDf$new_fn[rv_patient$mappingDf$file_info == "Target"]

      print("ref_files")
      print(ref_files)
      settings <- paste0(
        " -r ",
        paste0(
          ref_files,
          collapse = " "
        ),
        " -t ",
        targetFilePath,
        " -o ",
        paste0(
          outDir,
          timestamp,
          ".txt"
        ),
        " --align ",
        paste0(
          outDir,
          timestamp,
          "_alignment"
        )
      )


      if (!is.null(weights_file_path)) {
        settings <- paste0(
          settings,
          " -w ",
          normalizePath(weights_file_path)
        )
      }

      if (!is.null(extra_config_file_path)) {
        settings <- paste0(
          settings,
          " -config ",
          normalizePath(extra_config_file_path)
        )
      }

      print(settings)

      print("patient_mode-patient_incl_excl_list-dnd_target_incl")
      print(input[[ns("patient_incl_excl_list-dnd_target_incl")]])

      dnd_incl <- input[[ns("patient_incl_excl_list-dnd_target_incl")]]
      dnd_excl <- input[[ns("patient_incl_excl_list-dnd_target_excl")]]

      # ! note: include an exclude are mutually exclusive
      if (length(dnd_incl) > 0) {
        settings <- paste0(
          settings,
          " -include-terms ",
          paste(
            dnd_incl,
            collapse = " "
          )
        )
      }

      if (length(dnd_excl) > 0) {
        settings <- paste0(
          settings,
          " -exclude-terms ",
          paste(
            dnd_excl,
            collapse = " "
          )
        )
      }
      settings2 <- settings
      print("settings2")
      print(settings2)
      ref_prefixes <- rv_patient$mappingDf$id_prefixes[1:nrow((rv_patient$mappingDf)) - 1]
      print("ref_prefixes")
      print(ref_prefixes)

      # save rv_patient$mappingDf to a csv file
      if (!is.null(rv_patient$mappingDf) && nrow(rv_patient$mappingDf) > 2) {
        settings <- paste0(
          settings,
          " --append-prefixes ",
          paste(ref_prefixes, collapse = " ")
        )
      }

      phenoRankBin <- get_golem_options("PHENO_RANK_BIN")

      cmd <- paste0(
        phenoRankBin,
        settings
      )
      # run the perl script
      script_status <- system(
        paste0(
          cmd,
          " > ",
          outDir,
          timestamp,
          "_alignment.stdout"
        ),
        intern = TRUE
      )
      print(cmd)
      print("script_status")
      print(script_status)
      if (length(script_status) > 0 && script_status != 0) {
        stop("Perl script execution failed.")
      }

      print("run the perl script 2")
      settings2 <- gsub("\\s-t\\s", " ", settings2)

      all_prefixes <- rv_patient$mappingDf$id_prefixes
      cmd2 <- paste0(
        phenoRankBin,
        settings2,
        " --append-prefixes ",
        paste(all_prefixes, collapse = " ")
      )

      print("cmd2")
      print(cmd2)
      script_status <- system(
        cmd2,
        intern = TRUE
      )
      if (length(script_status) > 0 && script_status != 0) {
        stop("Perl script 2 execution failed.")
      }

      # TODO
      # note that dnd_incl and dnd_excl are mutually exclusive
      label <- "all toplevel terms"
      if (length(dnd_incl) > 0) {
        label <- paste("included toplevels:", paste(dnd_incl, collapse = ", "))
      }

      if (length(dnd_excl) > 0) {
        label <- paste("excluded toplevels:", paste(dnd_excl, collapse = ", "))
      }

      print("label")
      print(label)

      settings <- list(
        "include_terms" = dnd_incl,
        "exclude_terms" = dnd_excl,
        "input_format" = input_format
      )

      store_job_in_db(
        timestamp,
        rv_general$user_email,
        "patient",
        label,
        settings,
        db_conn
      )

      click("PatientHistorySidebar-btn_show_history")

      rv_patient$pastRunIds <- c(
        rv_patient$pastRunIds,
        timestamp
      )

      # accessm blast data to get the top levels
      bin_df <- readTxt(
        rv_general$user_dirs$output$pats_ranked,
        fileName_suffix = "_alignment.csv",
        runId = timestamp,
        sep = ";"
      )

      top_level_row <- bin_df[1, ]

      # in the top level row remove everything after the first dot
      top_level_row <- gsub("\\..*", "", top_level_row)
      top_level_row[1] < "top level"

      top_levels <- unique(top_level_row)

      print("top_levels")
      print(top_levels)
      print("before get_color_mapping")
      rv_patient$colors_mapping <- get_color_mapping(
        rv_general,
        timestamp,
        top_levels
      )

      # rv_patient$col_colors <- get_table_row_colors(
      #   rv_general$user_dirs$output$pats_ranked,
      #   timestamp,
      #   rv_general
      # )

      # TabHeader: Binary representation
      rv_patient$blastData <- mod_table_phenoBlast_server(
        "binaryRepresentationTable",
        rv_general,
        runId = timestamp,
        rv_patient = rv_patient
      )

      # TabHeader: Ranking
      rv_patient$rankingDf <- mod_table_phenoRanking_server(
        "phenoRankingTable",
        rv_general,
        runId = timestamp,
        rv_patient = rv_patient
      )

      # TabHeader: Hamming Distances Heatmap
      mod_heatmap_server(
        "heatmap",
        timestamp,
        rv_patient,
        rv_general,
        "patient"
      )

      # TabHeader: Multidimensional Scaling Scatter Plot
      mod_plot_mds_server(
        "mds_scatter",
        rv_general,
        runId = timestamp,
        rv = rv_patient,
        mode = "patient"
      )


      # blast_data <- renderbinaryRepresentationTable(timestamp)
      # ranking_df <- renderRankingTable(timestamp)
      # CardHeader: Target vs a selected reference individual
      # renderPhenoHeadsUpTable(timestamp, blast_data, ranking_df)

      # renderPlots(timestamp, rv_patient)
    })

    observeEvent(input$yamlEditorIdPrefixes, {
      req(input$yamlEditorIdPrefixes)
      req(rv_patient$mappingDf)

      lines <- strsplit(input$yamlEditorIdPrefixes, "\n", fixed = TRUE)[[1]]
      for (line in lines) {
        if (!grepl(":", line)) {
          print("no colon in line")
          showNotification(
            paste0(
              "A colon seems to be missing.",
              "Please use the following format: ",
              "<file_name>:<id_prefix>"
            ),
            type = "error"
          )
          rv_patient$idPrefixesYamlValid <- FALSE
          return()
        }
        file_name <- strsplit(line, ":", fixed = TRUE)[[1]][1]
        if (!file_name %in% rv_patient$mappingDf$original_fn) {
          print("file name not found")
          showNotification(
            paste0(
              "File name ",
              file_name,
              " not found in the uploaded files!"
            ),
            type = "error"
          )
          rv_patient$idPrefixesYamlValid <- FALSE
          return()
        } else {
          rv_patient$mappingDf[
            rv_patient$mappingDf$original_fn == file_name,
            "id_prefixes"
          ] <- strsplit(line, ":", fixed = TRUE)[[1]][2]
        }
      }
      result <- validateYAML(input$yamlEditorIdPrefixes)
      output$idPrefixesYamlErrorOutput <- renderText(result)
      rv_patient$idPrefixesYamlValid <- TRUE
    })

    # put these two in a function
    observeEvent(input$patient_conv_reference, {
      req(input$patient_conv_reference)

      expectedRowCount <- length(input$patient_conv_reference)

      observeConvertedDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_conversion,
        rv_general,
        "patient_conv_reference",
        "yamlEditorIdPrefixes",
        "yamlEditor_config",
        expectedRowCount
      )
    })

    # merge it with the one above
    observeEvent(input$patient_conv_target, {
      req(input$patient_conv_target)
      expectedRowCount <- length(input$patient_conv_target)
      observeConvertedDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_conversion,
        rv_general,
        "patient_conv_target",
        "yamlEditorIdPrefixes",
        "yamlEditor_config",
        expectedRowCount
      )
    })

    observeEvent(input$patient_beacon_api_reference, {
      req(input$patient_beacon_api_referece)
      expectedRowCount <- length(input$patient_beacon_api_reference)

      print("observeEvent input$patient_beacon_api_reference")
      print("expectedRowCount")
      print(expectedRowCount)

      observeBeaconApiDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_beacon_api,
        rv_general,
        "patient_beacon_api_reference",
        "yamlEditorIdPrefixes",
        expectedRowCount
      )
    })

    observeEvent(input$patient_beacon_api_target, {
      req(input$patient_beacon_api_target)
      expectedRowCount <- length(input$patient_beacon_api_target)
      observeBeaconApiDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_beacon_api,
        rv_general,
        "patient_beacon_api_target",
        "yamlEditorIdPrefixes",
        expectedRowCount
      )
    })

    observeEvent(input$patient_example_reference, {
      req(input$patient_example_reference)
      expectedRowCount <- length(input$patient_example_reference)
      observeExampleDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_input_examples,
        rv_general,
        "patient_example_reference",
        "yamlEditorIdPrefixes",
        expectedRowCount
      )
    })

    observeEvent(input$patient_example_target, {
      req(input$patient_example_target)
      expectedRowCount <- length(input$patient_sim_target)
      observeExampleDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_input_examples,
        rv_general,
        "patient_example_target",
        "yamlEditorIdPrefixes",
        expectedRowCount
      )
    })


    # TODO
    # merge the two functions into one
    observeEvent(input$patient_sim_reference, {
      req(input$patient_sim_reference)
      expectedRowCount <- length(input$patient_sim_reference)
      observeSimulatedDataChange(
        session,
        input,
        output,
        db_conn,
        rv_patient,
        rv_sim,
        rv_general,
        "patient_sim_reference",
        "yamlEditorIdPrefixes",
        expectedRowCount
      )
    })

    # merge the function with the one above
    observeEvent(input$patient_sim_target, {
      req(input$patient_sim_target)
      print("observeEvent input$patient_sim_target")
      expectedRowCount <- length(input$patient_sim_target)
      observeSimulatedDataChange(
        session,
        input,
        output,
        db_conn,
        rv_patient,
        rv_sim,
        rv_general,
        "patient_sim_target",
        "yamlEditorIdPrefixes",
        expectedRowCount
      )
    })

    observeEvent(input$simulatedRefsInputFormatRadio, {
      req(rv_patient$inputFormat)
      req(rv_patient$mappingDf)
      req(input$simulatedRefsInputFormatRadio)

      mapping_df <- create_new_mapping_df()

      print("observeEvent input$simulatedRefsInputFormatRadio")
      rv_patient$inputFormat <- input$simulatedRefsInputFormatRadio
      print(rv_patient$inputFormat)

      print("rv_sim$simulationId")
      print(rv_sim$simulationId)

      row <- data.frame(
        file_info = "Reference",
        original_fn = paste0(
          rv_sim$simulationId,
          ".",
          rv_patient$inputFormat,
          ".json"
        ),
        new_fn = normalizePath(
          paste0(
            rv_general$user_dirs$output$sim,
            "/",
            rv_sim$simulationId,
            ".",
            rv_patient$inputFormat,
            ".json"
          )
        ),
        id_prefixes = rv_patient$mappingDf$id_prefixes,
        simulatedData = TRUE,
        stringsAsFactors = FALSE
      )

      rv_patient$mappingDf <- rbind(mapping_df, row)

      # put this into a general function
      editor_val <- ""
      for (i in 1:nrow(rv_patient$mappingDf)) {
        editor_val <- paste0(
          editor_val,
          rv_patient$mappingDf$original_fn[i],
          ":",
          rv_patient$mappingDf$id_prefixes[i],
          "\n"
        )
      }
      updateAceEditor(
        session,
        "yamlEditorIdPrefixes",
        value = editor_val
      )
    })

    # generate_target_based_on_converted_data <- function(rv, rv_conversion, rv_general, convDataPath) {
    #   print("generate_target_based_on_converted_data")

    #   targetFilePath <- convDataPath
    #   print("targetFilePath")
    #   print(targetFilePath)

    #   jsonArray <- fromJSON(
    #     normalizePath(
    #       file.path(
    #         targetFilePath
    #       )
    #     ),
    #     simplifyDataFrame = FALSE,
    #   )
    #   jsonObj <- jsonArray[[1]]

    #   targetFilePath <- paste0(
    #     rv_general$user_dirs$output$conv,
    #     # get_golem_options("conversionOutputFolder"),
    #     "/",
    #     paste0(rv_conversion$id, "/"),
    #     rv_conversion$id,
    #     ".target.json"
    #   )
    #   file.create(targetFilePath)

    #   fileConn <- file(
    #     targetFilePath,
    #     open = "w"
    #   )
    #   writeLines(
    #     toJSON(
    #       jsonObj,
    #       pretty = TRUE,
    #       auto_unbox = TRUE
    #     ),
    #     con = fileConn
    #   )
    #   close(fileConn)
    #   return(targetFilePath)
    # }

    # generate_target_based_on_beacon_data <- function(rv, rv_beacon_api, rv_general, beaconApiDataPath) {
    #   print("generate_target_based_on_beacon_data")

    #   targetFilePath <- beaconApiDataPath
    #   print("targetFilePath")
    #   print(targetFilePath)

    #   jsonArray <- fromJSON(
    #     normalizePath(
    #       file.path(
    #         targetFilePath
    #       )
    #     ),
    #     simplifyDataFrame = FALSE,
    #   )
    #   jsonObj <- jsonArray[[1]]

    #   targetFilePath <- paste0(
    #     rv_general$user_dirs$output$beacon,
    #     "/",
    #     rv_beacon_api$queryId,
    #     ".target.json"
    #   )
    #   file.create(targetFilePath)

    #   fileConn <- file(
    #     targetFilePath,
    #     open = "w"
    #   )
    #   writeLines(
    #     toJSON(
    #       jsonObj,
    #       pretty = TRUE,
    #       auto_unbox = TRUE
    #     ),
    #     con = fileConn
    #   )
    #   close(fileConn)
    #   return(targetFilePath)
    # }

    # generate_target_based_on_example_data <- function(rv,
    #                                                   rv_input_examples,
    #                                                   rv_general,
    #                                                   exampleDataPath) {
    #   print("generate_target_based_on_example_data")

    #   targetFilePath <- exampleDataPath
    #   print("targetFilePath")
    #   print(targetFilePath)

    #   jsonArray <- fromJSON(
    #     normalizePath(
    #       file.path(
    #         targetFilePath
    #       )
    #     ),
    #     simplifyDataFrame = FALSE,
    #   )
    #   jsonObj <- jsonArray[[1]]

    #   targetFilePath <- paste0(
    #     rv_general$user_dirs$output$example,
    #     "/",
    #     rv_input_examples$retrievalId,
    #     ".target.pxf.json"
    #   )
    #   file.create(targetFilePath)

    #   fileConn <- file(
    #     targetFilePath,
    #     open = "w"
    #   )
    #   writeLines(
    #     toJSON(
    #       jsonObj,
    #       pretty = TRUE,
    #       auto_unbox = TRUE
    #     ),
    #     con = fileConn
    #   )
    #   close(fileConn)
    #   return(targetFilePath)
    # }

    # generate_target_based_on_simulated_data <- function(rv, rv_sim, rv_general, simDataPath) {
    #   print("generate_target_based_on_simulated_data")

    #   targetFilePath <- simDataPath
    #   print("targetFilePath")
    #   print(targetFilePath)

    #   jsonArray <- fromJSON(
    #     normalizePath(
    #       file.path(
    #         targetFilePath
    #       )
    #     ),
    #     simplifyDataFrame = FALSE,
    #   )
    #   jsonObj <- jsonArray[[1]]

    #   targetFilePath <- paste0(
    #     rv_general$user_dirs$output$sim,
    #     "/",
    #     rv_sim$simulationId,
    #     ".target.",
    #     rv$inputFormat,
    #     ".json"
    #   )
    #   file.create(targetFilePath)

    #   fileConn <- file(
    #     targetFilePath,
    #     open = "w"
    #   )
    #   writeLines(
    #     toJSON(
    #       jsonObj,
    #       pretty = TRUE,
    #       auto_unbox = TRUE
    #     ),
    #     con = fileConn
    #   )
    #   close(fileConn)
    #   return(targetFilePath)
    # }

    # might be needed for the cohort mode as well
    # create_new_mapping_df <- function() {
    #   return(data.frame(
    #     original_fn = character(),
    #     new_fn = character(),
    #     id_prefixes = character(),
    #     simulatedData = logical(),
    #     file_info = character(),
    #     stringsAsFactors = FALSE
    #   ))
    # }
  })
}
