#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom golem get_golem_options
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapWidget
#' @importFrom jsonlite read_json fromJSON
#' @importFrom lubridate ymd_hms
#' @import yaml
#' @importFrom utils URLdecode read.csv
#' @noRd

# good to know:
# here is how it might be possible to get the currently logged in user
# https://www.shinyproxy.io/documentation/configuration/#environment-variables
# SHINYPROXY_USERNAME: the name of the user, as used when logging in
# SHINYPROXY_USERGROUPS: the groups the authenticated
#                        user is a member of, as a comma-separated value

# Function to parse URL parameters
parseQueryString <- function(s) {
  if (is.null(s) || nchar(s) == 0) {
    return(list())
  }
  params <- strsplit(gsub("^\\?", "", s), "&", fixed = TRUE)[[1]]
  pairs <- strsplit(params, "=", fixed = TRUE)
  keys <- sapply(pairs, "[[", 1)
  values <- sapply(pairs, function(x) {
    if (length(x) == 1) {
      ""
    } else {
      URLdecode(x[[2]])
    }
  })
  as.list(setNames(values, keys))
}

app_server <- function(input, output, session) {

  # unsetting the LD_LIBRARY_PATH resolves
  # perl: symbol lookup error: perl: undefined symbol: PL_perl_destruct_level
  Sys.unsetenv("LD_LIBRARY_PATH")

  # TODO
  # reactlog should be conditionally enabled
  # reactlog_enable()

  # initialize reactive values
  rv_sim <- reactiveValues(
    dtInputs = NULL,
    simResult_bff = NULL,
    simResult_pxf = NULL,
    simulationId = NULL
  )

  rv_conversion <- reactiveValues(
    id = NULL,
    outputJson = NULL,
    configYaml = NULL
  )

  rv_patient <- reactiveValues(
    inputFormat = NULL,
    uploadedReferenceFile = NULL,
    uploadedTargetFile = NULL,
    mappingDf = NULL,
    alignmentDf = NULL,
    id = NULL,
    mdsPlot = NULL,
    useSimulatedReference = FALSE,
    useSimulatedTarget = FALSE,
    useConvertedReference = FALSE,
    useConvertedTarget = FALSE,
    idPrefixesYamlValid = NULL,
    runId = NULL,
    pastRunIds = c(),
    ht = NULL,
    blastData = NULL,
    rankingDf = NULL,
    allowedTerms = NULL
  )

  rv_cohort <- reactiveValues(
    inputFormat = NULL,
    useSimulatedCohort = NULL,
    # useConvertedCohort = NULL, is missing
    mappingDf = NULL,
    idPrefixesYamlValid = NULL,
    ht = NULL,
    mdsPlot = NULL,
    allowedTerms = NULL
  )

  # load modules
  db_conn <- mod_db_server("db")
  db_driver <- get_golem_options("dbDriver")
  print("dbDriver")
  print(db_driver)

  mod_sim_mode_server(
    "sim_mode",
    session,
    db_conn,
    db_driver,
    rv_sim
  )

  mod_conv_mode_server(
    "conv_mode",
    session,
    db_conn,
    rv_conversion
  )

  mod_patient_mode_server(
    "patient_mode",
    session,
    db_conn,
    rv_patient,
    rv_sim,
    rv_conversion
  )

  mod_cohort_mode_server(
    "cohort_mode",
    session,
    db_conn,
    rv_cohort,
    rv_sim,
    rv_conversion
  )

  historySidebars <- c(
    "SimulateHistorySidebar",
    "ConvertHistorySidebar",
    "PatientHistorySidebar",
    "CohortHistorySidebar"
  )

  lapply(historySidebars, function(sidebar) {
    mod_history_sidebar_server(
      sidebar,
      db_conn
    )
  })

  # TODO
  # it would be good to have the used
  # mode inside the ID
  # e.g. patient mode: pat142551
  # simulation mode: sim142552
  # cohort mode: coh42553
  # convert mode: conv42554

  if(get_golem_options("runWithDocker") == "True") {
    print("running with docker")
    Sys.setenv(LD_LIBRARY_PATH = paste(
      get_golem_options("LD_LIB_PATH"),
      Sys.getenv("LD_LIBRARY_PATH"),
      sep = ":"
    ))
    print(Sys.getenv("LD_LIBRARY_PATH"))
  } else {
    print("running without docker")
  }

  phenoSimBin <- get_golem_options("phenoSimBin")
  phenoRankBin <- get_golem_options("phenoRankBin")

  # maybe better to put this in a separate module(?)
  getPastRunResults <- function(mode,runId) {
    print("inside getPastRunResults")

    # TODO
    # maybe better to put this in the simulation module
    # with a flag
    # if history=True then run below
    if (mode == "sim") {
      print("inside getPastRunResults sim")
      rv_sim$simulationId <- runId
      output$simulationId <- renderText(paste0("RUN ID: ",runId))

      # query the database 
      query <- sprintf(
        "SELECT settings FROM jobs WHERE run_id = '%s' and mode = 'sim'",
        runId
      )

      res <- dbGetQuery(db_conn, query)
      settings <- fromJSON(res$settings)
      print("settings")
      print(settings)

      number_of_individuals <- as.numeric(settings$numberOfIndividuals)
      print("number_of_individuals")
      print(number_of_individuals)

      # TODO
      # add a get_golem_options wrapper to check if the
      # required option does not return NULL

      simulationOutputFolder <- get_golem_options("simulationOutputFolder")
      print("simulationOutputFolder")
      print(simulationOutputFolder)
      files <- list.files(
        simulationOutputFolder,
        pattern = paste0(runId,"*.(bff|pxf).json")
      )
      print("files")
      print(files)
      if (length(files) == 0) {
        print("no files found")
        return()
      }

      selectedOutputFormats <- c()
      for (file_name in files) {
        print("file_name")
        print(file_name)
        # check if the file is a bff or pxf file

        file_type <- gsub("\\d+\\.|\\.json", "", file_name)
        print("file_type")
        print(file_type)

        rv_sim[[paste0("simResult_", file_type)]] <- read_json(
          paste0(
            simulationOutputFolder,
            "/",
            file_name
          )
        )

        selectedOutputFormats <- c(
          selectedOutputFormats,
          toupper(file_type)
        )

        # TODO
        # get the arraySizeInput from the database
        # and pass it to the mod_json_viewer_server
        # because at the moment the history is failing

        mod_json_viewer_server(
          "sim_mode-json_viewer",
          selectedOutputFormats,
          rv_sim$simResult_bff,
          rv_sim$simResult_pxf,
          number_of_individuals
        )
      }
    } else if (mode == "patient") {
      rv_patient$runId <- runId
      output$phenoBlastRunId <- renderText(paste0("RUN ID: ",runId))

      outDir <- get_golem_options("patientModeOutputFolder")
      print("outDir")
      print(outDir)
      dirs <- list.dirs(outDir)
      print("dirs")
      print(dirs)

      # check if on of the directories contains the runId
      dir_bools <- sapply(dirs, function(dir) grepl(runId, dir))
      print("dir_bools")
      print(dir_bools)
      dirs <- dirs[dir_bools]

      print("dirs2")
      print(dirs)
      if (length(dirs) != 1) {
        # if it would be more than one directory
        # throw an error
        # if it would be 0 directories
        # show a message to the user
        print("no directory found")
        return()
      }
      dir <- dirs[1]

      # TabHeader: Binary representation
      rv_patient$blastData <- mod_table_phenoBlast_server(
        "patient_mode-phenoBlastTable",
        runId = runId,
        rv_patient = rv_patient
      )
      # TabHeader: Ranking
      rv_patient$rankingDf <- mod_table_phenoRanking_server(
        "patient_mode-phenoRankingTable",
        runId = runId,
        rv_patient = rv_patient
      )

      # TabHeader: Hamming Distances Heatmap
      mod_heatmap_server(
        "patient_mode_heatmap",
        runId,
        rv_patient,
        "patient",
      )

      rv_patient$mappingDf <- read.csv(
        paste0(
          get_golem_options("patientModeOutputFolder"),
          runId,
          "/",
          runId,
          "_mapping.csv"
        ),
      )

      # TabHeader: Multidimensional Scaling Scatter Plot
      mod_plot_mds_server(
        "patient_mode-mds_scatter",
        runId = runId,
        rv = rv_patient,
        mode = "patient"
      )
    } else if (mode == "cohort") {
      rv_cohort$runId <- runId
      output$phenoBlastCohortRunId <- renderText(paste0("RUN ID: ",runId))

      outDir <- get_golem_options("cohortModeOutputFolder")
      dirs <- list.dirs(outDir)
      print("dirs")
      print(dirs)

      # check if on of the directories contains the runId
      dirs_bools <- sapply(dirs, function(dir) grepl(runId, dir))
      dirs <- dirs[dirs_bools]
      if (length(dirs) != 1) {
        # if it would be more than one directory
        # throw an error
        # if it would be 0 directories
        # show a message to the user
        print("no directory found")
        return()
      }
      dir <- dirs[1]

      # query the database for the number of uploaded files
      query <- sprintf(
        "SELECT settings FROM jobs WHERE run_id = '%s' and mode = 'cohort'",
        runId
      )

      res <- dbGetQuery(db_conn, query)
      settings <- fromJSON(res$settings)

      uploaded_files_count <- 1

      if ("append_prefixes" %in% names(settings)) {
        uploaded_files_count <- length(settings$append_prefixes)
      }

      rv_cohort$mappingDf <- read.csv(
        paste0(
          get_golem_options("cohortModeOutputFolder"),
          runId,
          "/",
          runId,
          "_mapping.csv"
        ),
      )

      # TabHeader: Hamming Distances Heatmap
      mod_heatmap_server(
        "cohort_mode_heatmap",
        runId,
        rv_cohort,
        "cohort",
        uploaded_files_count = uploaded_files_count
      )

      # TabHeader: Multidimensional Scaling Scatter Plot
      mod_plot_mds_server(
        "cohort_mode-mds_scatter",
        runId = runId,
        rv = rv_cohort,
        mode = "cohort",
        uploaded_files_count = uploaded_files_count
      )
    } else if (mode == "conv") {
      rv_conversion$id <- runId
      output$conversionId <- renderText(paste0("RUN ID: ",runId))

      outDir <- paste0(
        get_golem_options("conversionOutputFolder")
      )

      dirs <- list.dirs(outDir)
      print("dirs")
      print(dirs)

      # check if on of the directories contains the runId
      dir_bools <- sapply(dirs, function(dir) grepl(runId, dir))
      dirs <- dirs[dir_bools]
      if (length(dirs) != 1) {
        # if it would be more than one directory
        # throw an error
        # if it would be 0 directories
        # show a message to the user
        print("no directory found")
        return()
      }

      dir <- dirs[1]

      jsonData <- read_json(
        paste0(
          dir,
          "/",
          runId,
          ".json"
        )
      )

      configVal <- paste(
        readLines(
          file.path(
            dir,
            paste0(runId,"_config.yaml")
          )
        ),
        collapse = "\n"
      )
      # for what is that needed?
      rv_conversion$outputJson <- jsonData
      rv_conversion$configYaml <- as.yaml(yaml.load(configVal))

      mod_conv_output_viewer_server(
        "conv_mode-conv_output_viewer",
        jsonData,
        configVal
      )
    }
  }

  # TODO
  # maybe better to put this in a separate module(?)
  observe({
    query <- parseQueryString(session$clientData$url_search)

    # TODO
    # try to get rid of the lubridate package
    if ("mode" %in% names(query)) {
      if ( "id" %in% names(query)) {
        date_time <- ymd_hms(query[["id"]], quiet = TRUE)
        mode <- query[["mode"]]
        currentId <- NULL
        if (mode == "sim") {
          currentId <- isolate(rv_sim$simulationId)
        } else if (mode == "patient") {
          currentId <- isolate(rv_patient$runId)
        } else if (mode == "cohort") {
          currentId <- isolate(rv_cohort$runId)
        }

        if (!is.null(currentId) && currentId == query[["id"]]) {
          print("run results are currently loaded")
          return()
        }

        if (!is.na(date_time)) {
          print("id is in the expected format")
          # !BUG
          # when running the application with Shinyproxy
          # the following error occurs:
          # "Link clicked with ID: 20231113121900"
          # [1] "id is in the expected format"
          # Warning: Error in if: argument is of length zero
          updateNavbarPage(session, "nav", query[["mode"]])
          print("after updateNavbarPage")
          print(query[["mode"]])
          getPastRunResults(query[["mode"]],query[["id"]])
        } else {
          # TODO
          # update the run ID field with information 
          # that the ID is not valid
        }
      }
      else {
        updateNavbarPage(
          session, "nav", query[["mode"]]
        )
      }
    }
  })

  observeEvent(input$nav, {
    id <- NULL
    if (input$nav == "sim") {
      id <- rv_sim$simulationId
    } else if (input$nav == "patient") {
      id <- rv_patient$runId
    } else if (input$nav == "cohort") {
      id <- rv_cohort$runId
    }
    #check
    runId <- id
    if (is.null(id)) {
      runId <- ""
    }
    session$sendCustomMessage(
      type = "changeURL",
      message = list(
        mode = input$nav,
        id = runId
      )
    )
  })

  #*  Note: the  redirection from the landing page buttons
  # is done here because it was not possible to run updateNavbarPage
  # from the landing page module
  observeEvent(input[["landing_page-navigateToSimulator"]], {
    updateNavbarPage(session, "nav", "sim")
  })

  observeEvent(input[["landing_page-navigateToExamples"]], {
    print("navigate to input examples")
    updateNavbarPage(session, "nav", "input_examples")
  })

  observeEvent(input[["landing_page-navigateToConverter"]], {
    updateNavbarPage(session, "nav", "conv")
  })

  observeEvent(input[["landing_page-navigateToPatientMode"]], {
    updateNavbarPage(session, "nav", "patient")
  })

  observeEvent(input[["landing_page-navigateToCohortMode"]], {
    updateNavbarPage(session, "nav", "cohort")
  })


  observeEvent(rv_patient$ht, {
    print("before rendering the heatmap")

    InteractiveComplexHeatmapWidget(
      input,
      output,
      session,
      ht_list=rv_patient$ht,
      output_id = "patient_mode-patient_heatmap",
      close_button = FALSE,
      layout = "1|23",
      width1 = "100%",
      width2 = "700px",
      width3 = "300px",
      height1 = "500px",
      height2 = "410px"
    )
  })

  # TODO
  # wrap below in a function
  observeEvent(rv_cohort$ht, {
    print("before rendering the cohort heatmap")
    InteractiveComplexHeatmapWidget(
      input,
      output,
      session,
      ht_list=rv_cohort$ht,
      output_id = "cohort_mode-cohort_heatmap",
      close_button = FALSE,
      layout = "1|23",
      width1 = "100%",
      width2 = "700px",
      width3 = "300px",
      height1 = "500px",
      height2 = "410px"
    )
  })
}
