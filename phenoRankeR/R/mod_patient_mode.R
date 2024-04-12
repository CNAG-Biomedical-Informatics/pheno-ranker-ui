#' Patient Mode Module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom gridlayout grid_container grid_card grid_place
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

patient_opts_layout = c(
  "         520px      ",
  "380px    rankerInput",
  "350px    configYamls",
  "350px    variables  "
)

mod_patient_mode_ui <- function(id){
  ns <- NS(id)
  grid_container(
    layout = mod_patient_mode_layout,
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
      grid_container(
        layout = patient_opts_layout,
        gap_size = "0px",
        grid_card(
          area = "rankerInput",
          card_body(
            tabsetPanel(
              id = ns("patientRankerTabsetPanel"),
              selected = "Reference(s)",
              type = "pills",
              tabPanel(
                title = "Reference(s)",
                card_body(
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
                    ),
                    # TODO as soon there is file uploaded
                    # select simulated data should be disabled
                    tabPanel(
                      title = "Simulated data",
                      grid_container(
                        layout = c(
                          "     1fr      1fr   ",
                          "150px dropdown radio "
                        ),
                        grid_place(
                          area = "dropdown",
                          selectInput(
                            ns("patient_sim_reference"),
                            "Select a simulated cohort",
                            choices = NULL
                          )
                        ),
                        grid_place(
                          area = "radio",
                          uiOutput(ns("simulatedRefsInputFormats"))
                        )
                      )
                    ),
                    tabPanel(
                      title = "Converted data",
                      card_body(
                        selectInput(
                          ns("patient_conv_reference"),
                          "Select a converted cohort",
                          choices = NULL
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                title = "Target",
                card_body(
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
                      title = "Simulated data",
                      card_body(
                        selectInput(
                          ns("patient_sim_target"),
                          "Select a simulated BFF/PXF",
                          choices = NULL
                        )
                      )
                    ),
                    tabPanel(
                      title = "Converted data",
                      card_body(
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
              )
            ),
            br(),
            br(),
            p("set individuals id prefix for each cohort"),
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
        verbatimTextOutput("phenoBlastRunId"),
        tabsetPanel(
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
        p("Version 0.0.0.9016")
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
  rv_sim,
  rv_conversion){
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <-session$ns
  moduleServer(id,function(input, output, session){

    mod_show_history_button_server(
      "PatientHistorySidebar",
      "patient",
      "PatientHistorySidebar",
      db_conn
    )

    mod_dnd_server(
      ns("patient_incl_excl_list"),
      rv_patient
    )

    mod_table_phenoBlast_server("binaryRepresentationTable")
    mod_table_phenoRanking_server("phenoRankingTable")
    mod_table_phenoHeadsUp_server("phenoHeadsUpTable")
    mod_plot_mds_server("mds_scatter")

    output$patient_heatmap <- renderUI({
      p("Click on Rank to generate the heatmap")
    })

    handleFileUpload <- function(input_id, rv, targetDir) {
      print("handleFileUpload")

      rank_input_dir <- get_golem_options("rankInputFolder")

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
        rv_patient=rv_patient
      )
    })

    observeEvent(input$patientRankerReferenceTabsetPanel, {
      print(input$patientRankerReferenceTabsetPanel)

      if (input$patientRankerReferenceTabsetPanel == "Simulated data") {
        observeTabChangeToSimulateData(
          input,
          session,
          db_conn,
          "patientRankerReferenceTabsetPanel",
          "patient_sim_reference"
        )
      }

      if (input$patientRankerReferenceTabsetPanel == "Converted data") {
        observeTabChangeToConvertedData(
          input,
          session,
          db_conn,
          "patientRankerReferenceTabsetPanel", 
          "patient_conv_reference"
        )
      }
    })

    observeEvent(input$patientRankerTargetTabsetPanel, {
      print("input$patientRankerTargetTabsetPanel")
      print(input$patientRankerTargetTabsetPanel)

      if (input$patientRankerReferenceTabsetPanel == "Simulated data") {
        observeTabChangeToSimulateData(
          input,
          session,
          db_conn,
          "patientRankerReferenceTabsetPanel",
          "patient_sim_target"
          )
      }

      if (input$patientRankerTargetTabsetPanel == "Converted data") {
        observeTabChangeToConvertedData(
          input,
          session,
          db_conn,
          "patientRankerTargetTabsetPanel",
          "patient_conv_target"
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

      rank_input_dir <- get_golem_options("patientRankInputRefsFolder")
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
          ":C",
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
          id_prefixes = paste0("C", i),
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

      mapping_df <- subset(mapping_df, file_info != "Target")
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

    set_input_paths <- function(rv, rv_sim, rv_conversion, mode) {
      print("set_input_paths")

      simDataPath <- paste0(
        get_golem_options("simulationOutputFolder"),
        rv_sim$simulationId,
        ".",
        rv$inputFormat,
        ".json"
      )

      convDataPath <- paste0(
        get_golem_options("conversionOutputFolder"),
        paste0(rv_conversion$id,"/"),
        rv_conversion$id,
        ".json"
      )

      upload_dir <- paste0(
        get_golem_options("rankInputFolder"),
        mode,
        "/"
      )
      
      file_paths <- c(
        "reference_file_path" = paste0(
          upload_dir,
          "references/",
          rv$uploadedReferenceFile
        ),
        "target_file_path" = paste0(
          upload_dir,
          "targets/",
          rv$uploadedTargetFile
        )
      )
      print(file_paths)

      if (mode == "patientMode" && rv$useSimulatedReference) {
        file_paths["reference_file_path"] <- simDataPath
      }

      if (mode == "patientMode" && rv$useConvertedReference) {
        file_paths["reference_file_path"] <- convDataPath
      }

      if (mode == "patientMode" && rv$useSimulatedTarget) {
        file_paths["target_file_path"] <- generate_target_based_on_simulated_data(
          rv,
          rv_sim,
          simDataPath
        )
      }

      if (mode == "patientMode" && rv$useConvertedTarget) {
        file_paths["target_file_path"] <- generate_target_based_on_converted_data(
          rv,
          rv_conversion,
          convDataPath
        )
      }

      file_paths <- lapply(file_paths, function(path) {
        info <- file.info(path)
        if (!info$isdir & !is.na(info$isdir)) {
          normalizePath(path)
        } else {
          print("NULL")
          print(path)
          NULL
        }
      })

      print("file_paths")
      print(file_paths)
      return(file_paths)
    }

    observeEvent(input$rankPatient, {
      print("rankPatient")

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

      paths <- set_input_paths(
        rv_patient,
        rv_sim,
        rv_conversion,
        "patientMode"
      )

      print("paths")
      print(paths)

      print("names(paths)")
      print(names(paths))

      if ("reference_file_path1" %in% names(paths)) {
        inputReferenceFilePath <- paths["reference_file_path1"]
      } else {
        inputReferenceFilePath <- paths["reference_file_path"]
      }
      inputTargetFilePath <- paths["target_file_path"]

      # TODO
      # put it in a extra files called errorHandlers.R
      if (is.null(inputReferenceFilePath[[1]]) || is.null(inputTargetFilePath[[1]])) {
        showNotification(
          "Please upload or select a simulated reference and target file!",
          type = "error"
        )
        return()
      }

      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      rv_patient$runId <- timestamp

      session$sendCustomMessage(
        type = "changeURL", 
        message = list(mode="patient",id=timestamp)
      )

      runId <- paste0(
        "RUN ID: ",
        timestamp
      )
        
      output$phenoBlastRunId <- renderText(runId)

      outDir <- paste0(
        paste0(
          get_golem_options("patientModeOutputFolder"),
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
          get_golem_options("patientModeOutputFolder"),
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
          get_golem_options("weightsUploadFolder"),
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
          get_golem_options("extraConfigsUploadFolder"),
          paste0(timestamp,"_config.yaml")
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

      ref_files <- rv_patient$mappingDf$new_fn[1:nrow((rv_patient$mappingDf)) - 1]

      print("ref_files")
      print(ref_files)
      settings <- paste0(
        " -r ",
        paste0(
          ref_files,
          collapse = " "
        ),
        " -t ",
        inputTargetFilePath,
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

      if(!is.null(extra_config_file_path)) {
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
        label <- paste("included toplevels:",paste(dnd_incl, collapse = ", "))
      }

      if (length(dnd_excl) > 0) {
        label <- paste("excluded toplevels:",paste(dnd_excl, collapse = ", "))
      }

      print("label")
      print(label)

      userId <- 1
      settings <- list()
      store_job_in_db(timestamp,userId,"patient",label, settings, db_conn)
      
      click("PatientHistorySidebar-btn_show_history")

      rv_patient$pastRunIds <- c(rv_patient$pastRunIds, timestamp)


      # TabHeader: Binary representation
      rv_patient$blastData <- mod_table_phenoBlast_server(
        "binaryRepresentationTable", 
        runId = timestamp,
        rv_patient = rv_patient
      )

      # TabHeader: Ranking
      rv_patient$rankingDf <- mod_table_phenoRanking_server(
        "phenoRankingTable", 
        runId = timestamp,
        rv_patient = rv_patient
      )

      # TabHeader: Hamming Distances Heatmap
      mod_heatmap_server(
        "heatmap",
        timestamp, 
        rv_patient,
        "patient"
      )

      # TabHeader: Multidimensional Scaling Scatter Plot
      mod_plot_mds_server(
        "mds_scatter",
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
      observeConvertedDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_conversion, 
        "patient_conv_reference", 
        "yamlEditorIdPrefixes",
        "yamlEditor_config"
        )
    })

    # merge it with the one above
    observeEvent(input$patient_conv_target, {
      req(input$patient_conv_target)
      observeConvertedDataChange(
        session,
        input,
        output,
        rv_patient,
        rv_conversion, 
        "patient_conv_target", 
        "yamlEditorIdPrefixes",
        "yamlEditor_config"
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

      # simulatedData_input_dir <- "./data/output/simulatedData/"
      simulatedData_input_dir <- get_golem_options("simulationOutputFolder")
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
            simulatedData_input_dir,
            rv_sim$simulationId,
            ".",
            rv_patient$inputFormat,
            ".json"
          )
        ),
        id_prefixes = "R",
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

    generate_target_based_on_converted_data <- function(rv, rv_conversion, convDataPath) {
      print("generate_target_based_on_converted_data")

      targetFilePath <- convDataPath
      print("targetFilePath")
      print(targetFilePath)

      jsonArray <- fromJSON(
        normalizePath(
          file.path(
            targetFilePath
          )
        ),
        simplifyDataFrame = FALSE,
      )
      jsonObj <- jsonArray[[1]]

      targetFilePath <- paste0(
        get_golem_options("conversionOutputFolder"),
        "/",
        paste0(rv_conversion$id, "/"),
        rv_conversion$id,
        ".target.json"
      )
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
      return(targetFilePath)
    }

    generate_target_based_on_simulated_data <- function(rv, rv_sim, simDataPath) {
      print("generate_target_based_on_simulated_data")

      targetFilePath <- simDataPath
      print("targetFilePath")
      print(targetFilePath)

      jsonArray <- fromJSON(
        normalizePath(
          file.path(
            targetFilePath
          )
        ),
        simplifyDataFrame = FALSE,
      )
      jsonObj <- jsonArray[[1]]

      targetFilePath <- paste0(
        get_golem_options("simulationOutputFolder"),
        # normalizePath("./data/output/simulatedData"),
        "/",
        rv_sim$simulationId,
        ".target.",
        rv$inputFormat,
        ".json"
      )
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
      return(targetFilePath)
    }

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