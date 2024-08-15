#' Cohort Mode Module
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
#' @importFrom jqr jq
#' @importFrom utils write.csv

cohort_layout = c(
  "           500px     1fr           40px                    ",
  "35px       btn       tabbedView    btn_show_cohort_history ",
  "1100px     opts      tabbedView    btn_show_cohort_history ",
  "1px        version   version       version                 "
)

cohort_opts_layout = c(
  "         500px        ",
  "380px    rankerInput  ",
  "340px    configYamls  ",
  "350px    variables    "
)

mod_cohort_mode_ui <- function(id){
  ns <- NS(id)
  version <- get_golem_options("packageVersion")

  grid_container(
    layout = cohort_layout,
    gap_size = "5px",
    grid_place(
      area = "btn",
      actionButton(
        ns("rankCohort"),
        "Rank",
        class = "btn btn-primary"
      )
    ),
    grid_card(
      area = "opts",
      card_header("Options"),
      grid_container(
        layout = cohort_opts_layout,
        gap_size = "0px",
        grid_card(
          area = "rankerInput",
          card_body(
            tabsetPanel(
              id = ns("cohortRankerTabsetPanel"),
              selected = "Upload",
              tabPanel(
                title = "Upload",
                card_body(
                  fileInput(
                    ns("cohortModeFiles"),
                    "Upload a BFF/PXF file for intra-cohort
                    or multiple for inter-cohort comparison",
                    multiple = TRUE,
                    accept = c(
                      ".json"
                    )
                  )
                )
              ),
              tabPanel(
                title = "Example data",
                card_body(
                  selectInput(
                    ns("cohort_input_examples"),
                    "Select example cohort(s)",
                    multiple = TRUE,
                    choices = NULL
                  )
                )
              ),
              tabPanel(
                title = "Simulated data",
                card_body(
                  grid_container(
                    layout = c(
                      "     1fr      1fr   ",
                      "150px dropdown radio "
                    ),
                    grid_place(
                      area = "dropdown",
                      selectInput(
                        ns("cohort_sim"),
                        "Select simulated cohort(s)",
                        multiple = TRUE,
                        choices = NULL
                      )
                    ),
                    grid_place(
                      area = "radio",
                      uiOutput(ns("simulatedCohortInputFormats"))
                    )
                  )
                )
              ),
              tabPanel(
                title = "Converted data",
                card_body(
                  selectInput(
                    ns("cohort_conv"),
                    "Select converted cohort(s)",
                    multiple = TRUE,
                    choices = NULL
                  )
                )
              )
            ),
            p("set individuals id prefix for each cohort"),
            aceEditor(
              ns("yamlCohortEditorIdPrefixes"),
              value = "",
              mode = "yaml",
              theme = "github",
              height = "60px"
            ),
            verbatimTextOutput(ns("cohortIdPrefixesErrorOutput")),
            mod_loader_ui(
              ns("loader_cohort_mode")
            )
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
                  ns("weightsCohortFile"), 
                  "Upload a weights yaml file",
                  multiple = FALSE,
                  accept = c(
                    ".yaml"
                  )
                ),
                p("or add the weights below:"),
                aceEditor(
                  ns("yamlCohortEditor_weights"),
                  value = weights_defaults,
                  mode = "yaml",
                  theme = "github",
                  height = "60px"
                ),
                verbatimTextOutput(ns("weightsYamlCohortsErrorOutput"))
              )
            ),
            tabPanel(
              title = "Extra Config",
              card_body(
                fileInput(
                  ns("extraConfigCohortFile"), 
                  "Upload a config yaml file",
                  multiple = FALSE,
                  accept = c(
                    ".yaml"
                  )
                ),
                p("or add the config below:"),
                aceEditor(
                  ns("yamlCohortEditor_config"),
                  value = "",
                  mode = "yaml",
                  theme = "github",
                  height = "60px"
                ),
                verbatimTextOutput("configYamlCohortsErrorOutput")
              )
            )
          )
        ),
        grid_place(
          area = "variables",
          mod_dnd_ui(ns("cohort_incl_excl_list"))
        )
      )
    ),
    grid_card(
      area = "tabbedView",
      card_header("Cohort Comparisons"),
      full_screen = TRUE,
      verbatimTextOutput(ns("phenoBlastCohortRunId")),

      tabsetPanel(
        id = ns("cohortRankingResults"),
        selected = "Hamming Distances Heatmap",
        tabPanel(
          title = "Hamming Distances Heatmap",
          card_body(
            htmlOutput(ns("cohort_heatmap")),
          )
        ),
        tabPanel(
          title = "Multi Dimensional Scaling Scatter Plot",
          mod_plot_mds_ui(ns("mds_scatter"))
        )
      )
    ),
    grid_place(
      area = "btn_show_cohort_history",
      mod_show_history_button_ui(ns("CohortHistorySidebar"))
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

create_settings_mapping_cohort_mode <- function(
ns,
weights_file_path,
extra_config_file_path,
input,
outDir,
timestamp,
rv_cohort) {

  settings <- list()

  # TODO
  # not only store the value but also the
  # the command line flag
  # e.g. -w for weights_file_path

  if (!is.null(weights_file_path)) {
    settings$weights_file_path <- normalizePath(weights_file_path)
  }

  if(!is.null(extra_config_file_path)) {
    settings$extra_config_file_path <- normalizePath(extra_config_file_path)
  }

  # ! note: include an exclude are mutually exclusive
  # TODO

  print("cohort_mode-patient_incl_excl_list-dnd_target_incl")
  print(input[[ns("cohort_incl_excl_list-dnd_target_incl")]])

  dnd_incl <- input[[ns("cohort_incl_excl_list-dnd_target_incl")]]
  dnd_excl <- input[[ns("cohort_incl_excl_list-dnd_target_excl")]]

  print("dnd_incl")
  print(dnd_incl)

  print("dnd_excl")
  print(dnd_excl)

  if (length(dnd_incl) > 0) {
    settings$include_terms <- dnd_incl
  }

  if (length(input$dn_excl) > 0) {

    # TODO
    # Throw an error and stop the execution
    # when the user tries to include and exclude terms
    # at the same time

    settings$exclude_terms <- dnd_excl
  }

  settings$out_dir <- paste0(outDir, timestamp, ".txt")

  # rv_cohort$mappingDf$new_fn <- paste0(outDir, rv_cohort$mappingDf$new_fn)
  settings$files <- rv_cohort$mappingDf

  uploaded_files_count <- nrow(rv_cohort$mappingDf)
  if (uploaded_files_count > 1) {
    settings$append_prefixes <- rv_cohort$mappingDf$id_prefixes
  }

  print(outDir)
  print(settings)

  return(settings)
}

# not used at the moment
# createSettingsStringCohortMode <- function(weights_file_path, input, rv_cohort, outDir) {
#   settings <- ""

#   if (!is.null(weights_file_path)) {
#     settings <- paste0(
#       settings,
#       " -w ",
#       normalizePath(weights_file_path)
#     )
#   }

#   # ! note: include an exclude are mutually exclusive
#   if (length(input$dnd_target_incl) > 0) {
#     settings <- paste0(
#       settings,
#       " -include-terms ",
#       paste(
#         input$dnd_target_incl,
#         collapse = " "
#       )
#     )
#   }

#   if (length(input$dnd_target_excl) > 0) {
#     settings <- paste0(
#       settings,
#       " -exclude-terms ",
#       paste(
#         input$dnd_target_excl,
#         collapse = " "
#       )
#     )
#   }

#   settings <- paste0(
#     settings,
#     " -r ",
#     paste(
#       rv_cohort$mappingDf$new_fn,
#       collapse = " "
#     ),
#     " -o ",
#     paste0(
#       outDir,
#       timestamp,
#       ".txt"
#     )
#   )
#   uploaded_files_count <- nrow(rv_cohort$mappingDf)
#   if (uploaded_files_count > 1) {
#     settings <- paste0(
#       settings,
#       " --append-prefixes ",
#       paste(
#         rv_cohort$mappingDf$id_prefixes,
#         collapse = " "
#       )
#     )
#   }
#   return(settings)
# }

mod_cohort_mode_server <- function(
  id,
  session,
  db_conn,
  rv_cohort,
  rv_input_examples,
  rv_sim,
  rv_conversion){
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <-session$ns
  moduleServer(id,function(input, output, session){

    submit_clicked <- reactive({
      input$rankCohort
    })

    mod_loader_server(
      "loader_cohort_mode",
      session,
      "rankCohort",
      submit_clicked,
      "Cohort Mode",
      "Please wait while the ranking is ongoing...",
      NULL
    )

    mod_show_history_button_server(
      "CohortHistorySidebar",
      "cohort",
      "CohortHistorySidebar",
      db_conn
    )

    mod_dnd_server(
      ns("cohort_incl_excl_list"),
      rv_cohort
    )

    mod_plot_mds_server("mds_scatter")

    output$cohort_heatmap <- renderUI({
      p("Click on Rank to generate the heatmap")
    })

    observeEvent(input$cohortModeFiles, {
      req(input$cohortModeFiles)

      rank_input_dir <- get_golem_options("cohortRankInputFolder")
      print("rank_input_dir")
      print(rank_input_dir)
      allowed_types <- c("json")

      input_format <- NULL
      cohorts <- list()
      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

      str_cohorts <- ""
      mapping_df <- data.frame(
        original_fn = character(),
        new_fn = character(),
        id_prefixes = character(),
        simulatedData = logical(),
        file_info = character(),
        stringsAsFactors = FALSE
      )

      print("input$cohortModeFiles")
      print(input$cohortModeFiles)

      total_individual_counts <- 0
      for (i in 1:nrow(input$cohortModeFiles)) {
        uploaded_file <- input$cohortModeFiles[i, ]
        print("uploaded_file")
        print(uploaded_file)

        print("uploaded_file$datapath")
        print(uploaded_file$datapath)

        file_con <- file(uploaded_file$datapath, "r")
        print("file_con")
        print(file_con)
        individual_counts <- as.numeric(jq(file_con, "length"))
        close(file_con)

        print("individual_counts")
        print(individual_counts)

        total_individual_counts <- total_individual_counts + individual_counts
        if (total_individual_counts > 1000) {
          showNotification(
            "The maximum number of individuals is 1000",
            type = "error"
          )
          reset("cohortModeFiles")
          return()
        }

        if (!(get_file_ext(uploaded_file$name) %in% allowed_types)) {
          showNotification("Invalid file type!", type = "error")
          reset("cohortModeFiles")
          return(NULL)
        }

        # TODO
        # put this in an extra function
        json_data <- fromJSON(readLines(uploaded_file$datapath))
        rv_cohort$inputFormat <- "bff"
        if ("subject" %in% names(json_data)) {
          rv_cohort$inputFormat <- "pxf"
        }

        # put this in an extra function inside validators.R
        if (is.null(input_format)) {
          input_format <- rv_cohort$inputFormat
        } else {
          if (input_format != rv_cohort$inputFormat) {
            showNotification(
              "Please upload files with the same pheno-clinical data format!",
              type = "error"
            )
            return(NULL)
          }
        }
        print("file")
        print(uploaded_file)
        print("file$name")
        print(uploaded_file$name)

        # add file name to the str_cohorts
        str_cohorts <- paste0(
          str_cohorts,
          uploaded_file$name,
          ":C",
          i,
          "\n"
        )
        print("str_cohorts")
        print(str_cohorts)

        fn <- paste0(
          timestamp,
          "_",
          i,
          ".",
          rv_cohort$inputFormat,
          ".json"
        )
        cohorts[[i]] <- fn

        file_path <- file.path(
          rank_input_dir,
          fn
        )

        print("file_path")
        print(file_path)

        file.copy(
          uploaded_file$datapath,
          file_path
        )

        row <- data.frame(
          original_fn = uploaded_file$name,
          new_fn = normalizePath(paste0(rank_input_dir, "/", fn)),
          id_prefixes = paste0("C", i),
          simulatedData = FALSE,
          file_info = "Cohort",
          stringsAsFactors = FALSE
        )
        mapping_df <- rbind(mapping_df, row)
      }

      updateAceEditor(
        session,
        "yamlCohortEditorIdPrefixes",
        value = str_cohorts
      )
      rv_cohort$mappingDf <- mapping_df
    })

    observeEvent(input$yamlCohortEditorIdPrefixes, {
      req(input$yamlCohortEditorIdPrefixes)
      req(rv_cohort$mappingDf)

      lines <- strsplit(input$yamlCohortEditorIdPrefixes, "\n", fixed = TRUE)[[1]]
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
          rv_cohort$idPrefixesYamlValid <- FALSE
          return()
        }
        file_name <- strsplit(line, ":", fixed = TRUE)[[1]][1]
        if (!file_name %in% rv_cohort$mappingDf$original_fn) {
          print("file name not found")
          showNotification(
            paste0(
              "File name ",
              file_name,
              " not found in the uploaded files!"
            ),
            type = "error"
          )
          rv_cohort$idPrefixesYamlValid <- FALSE
          return()
        } else {
          rv_cohort$mappingDf[
            rv_cohort$mappingDf$original_fn == file_name,
            "id_prefixes"
          ] <- strsplit(line, ":", fixed = TRUE)[[1]][2]
        }
      }
      result <- validateYAML(input$yamlCohortEditorIdPrefixes)
      print("validateYAML result")
      print(result)
      output$cohortIdPrefixesErrorOutput <- renderText(result)
      rv_cohort$idPrefixesYamlValid <- TRUE
    })

    # TODO
    # merge input$rankCohort and input$rankPatient
    observeEvent(input$cohortRankingBtnClicked, {
      if (is.null(rv_cohort$mappingDf)) {
        showNotification(
          "Please upload at least one file or select example/simulated data",
          type = "error"
        )
        return(NULL)
      }

      updateTabsetPanel(
        session,
        "cohortRankingResults",
        "Hamming Distances Heatmap"
      )

      output$cohort_heatmap <- renderUI({
        p("Generating heatmap...")
      })

      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      rv_cohort$runId <- timestamp

      session$sendCustomMessage(
        type = "changeURL",
        message = list(
          mode = "cohort",
          id = timestamp
        )
      )
      runId <- paste0(
        "RUN ID: ",
        timestamp
      )
      output$phenoBlastCohortRunId <- renderText(runId)

      weights_file_path <- NULL
      if (input$yamlCohortEditor_weights != "") {
        fn <- paste0(
          timestamp,
          ".yaml"
        )
        weights_file_path <- file.path(
          get_golem_options("weightsUploadFolder"),
          fn
        )
        writeLines(
          input$yamlCohortEditor_weights,
          weights_file_path
        )
      }

      extra_config_file_path <- NULL
      if (input$yamlCohortEditor_config != "") {
        extra_config_file_path <- file.path(
          get_golem_options("extraConfigsUploadFolder"),
          paste0(timestamp, "_config.yaml")
        )

        writeLines(
          input$yamlCohortEditor_config,
          extra_config_file_path
        )
      }

      outDir <- paste0(
        get_golem_options("cohortModeOutputFolder"),
        timestamp,
        "/"
      )

      dir.create(
        outDir,
        recursive = TRUE
      )

      write.csv(
        rv_cohort$mappingDf,
        file = paste0(
          get_golem_options("cohortModeOutputFolder"),
          timestamp,
          "/",
          timestamp,
          "_mapping.csv"
        ),
        row.names = FALSE
      )

      # settings <- createSettingsStringCohortMode(
      #   weights_file_path,
      #   input,
      #   rv_cohort,
      #   outDir
      # )

      print("before create_settings_mapping_cohort_mode")

      # rv_cohort$mappingDf$new_fn <- paste0(outDir, rv_cohort$mappingDf$new_fn)
      # when commenting below in the app crashes
      settingsMapping <- create_settings_mapping_cohort_mode(
        ns,
        weights_file_path,
        extra_config_file_path,
        input,
        outDir,
        timestamp,
        rv_cohort
      )

      # TODO
      # create the settings string from the settings list
      settings <- ""
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

      print("cohort_mode-patient_incl_excl_list-dnd_target_incl")
      print(input[[ns("cohort_incl_excl_list-dnd_target_incl")]])

      dnd_incl <- input[[ns("cohort_incl_excl_list-dnd_target_incl")]]
      dnd_excl <- input[[ns("cohort_incl_excl_list-dnd_target_excl")]]

      print("dnd_incl2")
      print(dnd_incl)

      print("dnd_excl2")
      print(dnd_excl)

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

      print(outDir)
      settings <- paste0(
        settings,
        " -r ",
        paste(
          rv_cohort$mappingDf$new_fn,
          collapse = " "
        ),
        " -o ",
        paste0(
          outDir,
          timestamp,
          ".txt"
        )
      )

      uploaded_files_count <- nrow(rv_cohort$mappingDf)
      if (uploaded_files_count > 1) {
        settings <- paste0(
          settings,
          " --append-prefixes ",
          paste(
            rv_cohort$mappingDf$id_prefixes,
            collapse = " "
          )
        )
      }
      # TODO
      # writing to log file is missing


      phenoRankBin <- get_golem_options("PHENO_RANK_BIN")
      # run the perl script
      cmd <- paste0(
        phenoRankBin,
        settings
      )
      print(cmd)

      script_status <- system(cmd, intern = TRUE)
      if (length(script_status) > 0 && script_status != 0) {
        stop("Perl script execution failed.")
      }

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
      # settings <- list()
      store_job_in_db(
        timestamp,
        userId,
        "cohort",
        label,
        settingsMapping,
        db_conn
      )

      click("CohortHistorySidebar-btn_show_history")

      # TabHeader: Hamming Distances Heatmap
      mod_heatmap_server(
        "heatmap",
        timestamp,
        rv_cohort,
        "cohort",
        uploaded_files_count = uploaded_files_count
      )

      # TabHeader: Multi Dimensional Scaling Scatter Plot
      mod_plot_mds_server(
        "mds_scatter",
        timestamp,
        rv = rv_cohort,
        mode = "cohort",
        uploaded_files_count = uploaded_files_count
      )
    })

    observeEvent(input$weightsCohortFile, {
      req(input$weightsCohortFile)

      allowed_types <- c("yaml")
      if (!(get_file_ext(input$weightsCohortFile$name) %in% allowed_types)) {
        showNotification("Invalid file type!", type = "error")
        reset("weightsCohortFile")
        return()
      }

      file_data <- paste(
        readLines(
          input$weightsCohortFile$datapath
        ),
        collapse = "\n"
      )

      yamlValid <- validateYAML(file_data)
      if (yamlValid != "YAML is valid") {
        showNotification(yamlValid, type = "error")
        output$weightsYamlCohortsErrorOutput <- renderText(yamlValid)
        updateAceEditor(session, "yamlCohortEditor_weights", value = "")
        return()
      }
      output$weightsYamlCohortsErrorOutput <- renderText(yamlValid)

      updateAceEditor(
        session,
        "yamlCohortEditor_weights",
        value = as.yaml(yaml.load(file_data))
      )
    })

    observeEvent(input$yamlCohortEditor_weights, {
      result <- validateYAML(input$yamlCohortEditor_weights)
      output$weightsYamlCohortsErrorOutput <- renderText(result)
    })

    observeEvent(input$extraConfigCohortFile, {
      req(input$extraConfigCohortFile)

      allowed_types <- c("yaml")
      if (!(get_file_ext(input$extraConfigCohortFile$name) %in% allowed_types)) {
        showNotification("Invalid file type!", type = "error")
        reset("extraConfigCohortFile")
        return()
      }

      file_data <- paste(
        readLines(
          input$extraConfigCohortFile$datapath
        ),
        collapse = "\n"
      )

      yamlValid <- validateYAML(file_data)
      if (yamlValid != "YAML is valid") {
        showNotification(yamlValid, type = "error")
        output$configYamlCohortsErrorOutput <- renderText(yamlValid)
        updateAceEditor(session, "yamlCohortEditor_config", value = "")
        return()
      }
      output$configYamlCohortsErrorOutput <- renderText(yamlValid)

      updateAceEditor(
        session,
        "yamlCohortEditor_config",
        value = as.yaml(yaml.load(file_data))
      )
    })

    observeEvent(input$yamlCohortEditor_config, {
      result <- validateYAML(input$yamlCohortEditor_config)
      output$configYamlCohortsErrorOutput <- renderText(result)

      print("input$yamlCohortEditor_config")
      data <- yaml.load(input$yamlCohortEditor_config)
      print(data)

      rv_cohort$allowedTerms <- data$allowed_terms
    })

    observeEvent(input$simulatedCohortInputFormatRadio, {
      req(rv_cohort$inputFormat)
      req(rv_cohort$mappingDf)
      req(input$simulatedCohortInputFormatRadio)

      mapping_df <- create_new_mapping_df()

      print("observeEvent input$simulatedCohortInputFormatRadio")
      rv_cohort$inputFormat <- input$simulatedCohortInputFormatRadio

      simulatedData_input_dir <- get_golem_options("simulationOutputFolder")
      row <- data.frame(
        file_info = "Cohort",
        original_fn = paste0(
          rv_sim$simulationId,
          ".",
          rv_cohort$inputFormat,
          ".json"
        ),
        new_fn = normalizePath(
          paste0(
            simulatedData_input_dir,
            rv_sim$simulationId,
            ".",
            rv_cohort$inputFormat,
            ".json"
          )
        ),
        id_prefixes = "C",
        simulatedData = TRUE,
        stringsAsFactors = FALSE
      )

      rv_cohort$mappingDf <- rbind(mapping_df, row)

      # put this into a general function
      editor_val <- ""
      for (i in 1:nrow(rv_cohort$mappingDf)) {
        editor_val <- paste0(
          editor_val,
          rv_cohort$mappingDf$original_fn[i],
          ":",
          rv_cohort$mappingDf$id_prefixes[i],
          "\n"
        )
      }
      updateAceEditor(
        session,
        "yamlCohortEditorIdPrefixes",
        value = editor_val
      )
    })

    observeEvent(input$cohortRankerTabsetPanel, {
      print("input$cohortRankerTabsetPanel")
      print(input$cohortRankerTabsetPanel)

      if(input$cohortRankerTabsetPanel == "Example data") {
        observeTabChangeToExampleData(
          input,
          session,
          db_conn,
          "cohortRankerTabsetPanel",
          "cohort_input_examples"
        )
      }

      if (input$cohortRankerTabsetPanel == "Simulated data") {
        observeTabChangeToSimulateData(
          input,
          session,
          db_conn,
          "cohortRankerTabsetPanel",
          "cohort_sim"
        )
      }

      if (input$cohortRankerTabsetPanel == "Converted data") {
        observeTabChangeToConvertedData(
          input,
          session,
          db_conn,
          "cohortRankerTabsetPanel",
          "cohort_conv"
        )
      }
    })

    observeEvent(input$cohort_input_examples, {
      req(input$cohort_input_examples)
      rv_cohort$inputFormat <- "pxf"

      expectedRowCount <- length(input$cohort_input_examples)
      print("expectedRowCount")

      observeExampleDataChange(
        session,
        input,
        output,
        rv_cohort,
        rv_input_examples,
        "cohort_input_examples",
        "yamlCohortEditorIdPrefixes",
        expectedRowCount
      )
    })

    observeEvent(input$cohort_sim, {
      req(input$cohort_sim)
      print("observeEvent input$cohort_sim")
      print("input$cohort_sim")
      print(input$cohort_sim)

      expectedRowCount <- length(input$cohort_sim)
      print("expectedRowCount")
      print(expectedRowCount)

      observeSimulatedDataChange(
        session,
        input,
        output,
        db_conn,
        rv_cohort,
        rv_sim,
        "cohort_sim",
        "yamlCohortEditorIdPrefixes",
        expectedRowCount
      )
    })

    observeEvent(input$cohort_conv, {
      req(input$cohort_conv)
      expectedRowCount <- length(input$cohort_conv)
      observeConvertedDataChange(
        session,
        input,
        output,
        rv_cohort,
        rv_conversion,
        "cohort_conv",
        "yamlCohortEditorIdPrefixes",
        "yamlCohortEditor_config",
        expectedRowCount
      )
    })
  })
}