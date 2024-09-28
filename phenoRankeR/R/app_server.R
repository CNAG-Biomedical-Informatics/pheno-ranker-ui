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

  # initialize the output folder for the new user
  shinyproxy <- get_golem_options("shinyProxyDeployed")
  keycloakSecured <- get_golem_options("keycloakSecured")

  playground_user <- get_golem_options("playgroundDummyEmail")
  user_email <- ""
  if (shinyproxy && keycloakSecured && Sys.getenv("SHINYPROXY_USERNAME") != playground_user) {
    user_email <- Sys.getenv("SHINYPROXY_USERNAME")
  }

  if (user_email == "") {
    user_email <- playground_user
  }

  # create the folders for the user
  # if they do not exist

  parent_dir <- get_golem_options("userDataDir")
  sub_dirs <- get_golem_options("subDirs")

  print("parent_dir")
  print(parent_dir)

  print("sub_dirs")
  print(sub_dirs)

  sub_dirs_output <- sub_dirs$output
  sub_dirs_uploads <- sub_dirs$uploads

  user_dir <- paste0(parent_dir, user_email, "/")

  # create a mapping containing all dirs that need to be created
  # and their corresponding subdirectories in the user folder
  # using the full path

  all_dirs <- list(
    output = sub_dirs$output,
    uploads = sub_dirs$uploads
  )

  # loop through the keys and add user_dir in front of the subdirectories
  for (key in names(all_dirs$output)) {
    all_dirs$output[[key]] <- paste0(user_dir, "output/", all_dirs$output[[key]])
  }

  for (key in names(all_dirs$uploads)) {
    all_dirs$uploads[[key]] <- paste0(user_dir, "uploads/", all_dirs$uploads[[key]])
  }

  print("all_dirs")
  print(all_dirs)

  # for (sub_dir in sub_dirs_uploads) {
  #   all_dirs <- c(all_dirs, paste0(user_dir, "uploads/", sub_dir))
  # }

  # for (sub_dir in sub_dirs_output) {
  #   all_dirs <- c(all_dirs, paste0(user_dir, "output/", sub_dir))
  # }

  # print("all_dirs")
  # print(all_dirs)


  # sub_dirs_uploads <- c(
  #   cfg = "config",
  #   onts = "ontologies",
  #   pat_mode_refs = "rankInput/patientMode/references",
  #   pats_mode_targets = "rankInput/patientMode/targets",
  #   cohort_mode_cohorts = "rankInput/cohortMode/cohorts",
  #   weights = "weights"
  # )

  # sub_dirs_output <- c(
  #   sim = "simulatedData",
  #   examples = "inputExamples",
  #   conv = "convertedData",
  #   pats_ranked = "rankedPatients",
  #   cohorts_ranked = "rankedCohortMatrixes"
  # )

  if (!dir.exists(user_dir)) {
    print("creating user folder")
    dir.create(user_dir)
  }

  # create the subfolders using all_dirs
  for (key in names(all_dirs$output)) {
    folder <- all_dirs$output[[key]]
    print(folder)
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }
  }

  for (key in names(all_dirs$uploads)) {
    folder <- all_dirs$uploads[[key]]
    print(folder)
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }
  }

  # print("creating subfolders - uploads")
  # for (sub_dir in sub_dirs_uploads) {
  #   folder <- paste0(user_dir, "uploads/", sub_dir)
  #   print(folder)
  #   if (!dir.exists(folder)) {
  #     dir.create(folder, recursive = TRUE)
  #   }
  # }

  # print("creating subfolders - output")
  # for (sub_dir in sub_dirs_output) {
  #   folder <- paste0(user_dir, "output/", sub_dir)
  #   print(folder)
  #   if (!dir.exists(folder)) {
  #     dir.create(folder, recursive = TRUE)
  #   }
  # }



  # TODO
  # reactlog should be conditionally enabled
  # reactlog_enable()

  # initialize reactive values
  rv_general <- reactiveValues(
    user_dirs = all_dirs,
    user_email = user_email
  )

  rv_beacon_api <- reactiveValues(
    queryId = NULL,
    beaconApiResults = NULL
  )

  rv_input_examples <- reactiveValues(
    retrievalId = NULL,
    inputExamples = NULL
  )

  rv_sim <- reactiveValues(
    dtInputs = NULL,
    simResult_bff = NULL,
    simResult_pxf = NULL,
    simulationId = NULL # note this could be also multiple ids
  )

  rv_conversion <- reactiveValues(
    id = NULL,
    outputJson = NULL,
    configYaml = NULL,
    numRows = NULL
  )

  rv_patient <- reactiveValues(
    inputFormat = NULL,
    uploadedReferenceFile = NULL,
    uploadedTargetFile = NULL,
    mappingDf = NULL,
    alignmentDf = NULL,
    id = NULL,
    mdsPlot = NULL,
    useBeaconReference = FALSE,
    useBeaconTarget = FALSE,
    useExampleReference = FALSE,
    useExampleTarget = FALSE,
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
    allowedTerms = NULL,
    col_colors = NULL
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

  mod_input_examples_page_server(
    "input_examples",
    session,
    db_conn,
    db_driver,
    rv_input_examples,
    rv_general
  )

  mod_beacon_api_page_server(
    "beacon_api",
    session,
    db_conn,
    db_driver,
    rv_beacon_api,
    rv_general
  )

  mod_sim_mode_server(
    "sim_mode",
    session,
    db_conn,
    db_driver,
    rv_sim,
    rv_general
  )

  mod_conv_mode_server(
    "conv_mode",
    session,
    db_conn,
    rv_conversion,
    rv_general
  )

  mod_patient_mode_server(
    "patient_mode",
    session,
    db_conn,
    rv_patient,
    rv_beacon_api,
    rv_input_examples,
    rv_sim,
    rv_conversion,
    rv_general
  )

  mod_cohort_mode_server(
    "cohort_mode",
    session,
    db_conn,
    rv_cohort,
    rv_beacon_api,
    rv_input_examples,
    rv_sim,
    rv_conversion,
    rv_general
  )

  historySidebars <- c(
    "SimulateHistorySidebar",
    "ConvertHistorySidebar",
    "PatientHistorySidebar",
    "CohortHistorySidebar",
    "InputExamplesRetrievalHistorySidebar",
    "BeaconApiHistorySidebar"
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

  # TODO
  # True should not be a string
  if (get_golem_options("runWithDocker") == "True") {
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
  getPastRunResults <- function(query, rv_general) {
    print("inside getPastRunResults")

    mode <- query[["mode"]]
    print("mode")
    print(mode)

    runId <- query[["id"]]
    print("runId")
    print(runId)

    # TODO
    # maybe better to put this in the simulation module
    # with a flag
    # if history=True then run below

    if (mode == "beacon_api") {
      print("inside getPastRunResults beacon_api")

      print(rv_beacon_api)
      rv_beacon_api$queryId <- runId

      output$queryId <- renderText(paste0("QUERY ID: ", runId))

      # query the database
      query <- sprintf(
        "SELECT settings FROM jobs WHERE run_id = '%s' and mode = 'beacon_api'",
        runId
      )

      print("before query")
      res <- dbGetQuery(db_conn, query)
      print("after query")
      print("res")
      print(res)

      settings <- fromJSON(res$settings)
      print("settings")
      print(settings)

      number_of_individuals <- as.numeric(settings$numberOfIndividuals)

      beaconApiOutputFolder <- rv_general$user_dirs$output$beacon
      print("beaconApiOutputFolder")
      print(beaconApiOutputFolder)

      files <- list.files(
        beaconApiOutputFolder,
        pattern = paste0(runId, "*.bff.json")
      )

      if (length(files) == 0) {
        print("no files found")
        return()
      }

      file_type <- "bff"

      rv_beacon_api$beaconApiResult <- read_json(
        paste0(
          beaconApiOutputFolder,
          "/",
          files[1]
        )
      )

      mod_json_viewer_server(
        "beacon_api-json_viewer_beacon_api",
        toupper(file_type),
        rv_beacon_api$beaconApiResult,
        rv_beacon_api$beaconApiResult,
        number_of_individuals
      )
    }


    if (mode == "input_examples") {
      print("inside getPastRunResults input_examples")
      rv_input_examples$retrievalId <- runId
      output$retrievalId <- renderText(paste0("RETRIEVAL ID: ", runId))

      print("runId")
      print(runId)

      # query the database
      query <- sprintf(
        "SELECT settings FROM jobs WHERE run_id = '%s' and mode = 'input_examples'",
        runId
      )

      res <- dbGetQuery(db_conn, query)

      print("res")
      print(res)

      settings <- fromJSON(res$settings)

      print("settings")
      print(settings)

      number_of_individuals <- as.numeric(settings$numberOfIndividuals)
      print("number_of_individuals")
      print(number_of_individuals)

      # inputExamplesOutputFolder <- get_golem_options("inputExamplesOutputFolder")
      inputExamplesOutputFolder <- rv_general$user_dirs$output$examples


      files <- list.files(
        inputExamplesOutputFolder,
        pattern = paste0(runId, "*.(bff|pxf).json")
      )

      print("files")
      print(files)

      if (length(files) == 0) {
        print("no files found")
        return()
      }

      file_type <- "pxf"

      rv_input_examples$inputExamples <- read_json(
        paste0(
          inputExamplesOutputFolder,
          "/",
          files[1]
        )
      )

      mod_json_viewer_server(
        "input_examples-json_viewer_input_examples",
        toupper(file_type),
        rv_input_examples$inputExamples,
        rv_input_examples$inputExamples,
        number_of_individuals
      )
    }


    if (mode == "sim") {
      print("inside getPastRunResults sim")
      rv_sim$simulationId <- runId
      output$simulationId <- renderText(paste0("RUN ID: ", runId))

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

      # simulationOutputFolder <- get_golem_options("simulationOutputFolder")
      simulationOutputFolder <- rv_general$user_dirs$output$sim

      print("simulationOutputFolder")
      print(simulationOutputFolder)
      files <- list.files(
        simulationOutputFolder,
        pattern = paste0(runId, "*.(bff|pxf).json")
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

        mod_json_viewer_server(
          "sim_mode-json_viewer_sim_mode",
          selectedOutputFormats,
          rv_sim$simResult_bff,
          rv_sim$simResult_pxf,
          number_of_individuals
        )
      }
    } else if (mode == "patient") {
      rv_patient$runId <- runId
      output$phenoBlastRunId <- renderText(paste0("RUN ID: ", runId))

      # outDir <- get_golem_options("patientModeOutputFolder")
      outDir <- rv_general$user_dirs$output$pats_ranked

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
        "patient_mode-binaryRepresentationTable",
        runId = runId,
        rv_patient = rv_patient
      )
      # TabHeader: Ranking
      rv_patient$rankingDf <- mod_table_phenoRanking_server(
        "patient_mode-phenoRankingTable",
        rv_general,
        runId = runId,
        rv_patient = rv_patient
      )

      # TabHeader: Hamming Distances Heatmap
      mod_heatmap_server(
        "patient_mode_heatmap",
        runId,
        rv_patient,
        rv_general,
        "patient"
      )

      rv_patient$mappingDf <- read.csv(
        paste0(
          rv_general$user_dirs$output$pats_ranked,
          "/",
          # get_golem_options("patientModeOutputFolder"),
          runId,
          "/",
          runId,
          "_mapping.csv"
        ),
      )

      # TabHeader: Multidimensional Scaling Scatter Plot
      mod_plot_mds_server(
        "patient_mode-mds_scatter",
        rv_general,
        runId = runId,
        rv = rv_patient,
        mode = "patient"
      )
    } else if (mode == "cohort") {
      rv_cohort$runId <- runId
      output$phenoBlastCohortRunId <- renderText(paste0("RUN ID: ", runId))

      # outDir <- get_golem_options("cohortModeOutputFolder")
      outDir <- rv_general$user_dirs$output$cohorts_ranked

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
          rv_general$user_dirs$output$cohorts_ranked,
          "/",
          # get_golem_options("cohortModeOutputFolder"),
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
        rv_general,
        "cohort",
        uploaded_files_count = uploaded_files_count
      )

      # TabHeader: Multidimensional Scaling Scatter Plot
      mod_plot_mds_server(
        "cohort_mode-mds_scatter",
        rv_general,
        runId = runId,
        rv = rv_cohort,
        mode = "cohort",
        uploaded_files_count = uploaded_files_count
      )
    } else if (mode == "conv") {
      rv_conversion$id <- runId
      output$conversionId <- renderText(paste0("RUN ID: ", runId))

      # outDir <- paste0(
      #   get_golem_options("conversionOutputFolder")
      # )

      outDir <- rv_general$user_dirs$output$conv

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
            paste0(runId, "_config.yaml")
          )
        ),
        collapse = "\n"
      )
      # for what is that needed?
      rv_conversion$outputJson <- jsonData
      rv_conversion$configYaml <- as.yaml(yaml.load(configVal))

      # print("jsonData")
      # print(jsonData)

      # TODO
      # it would be better to have the number of rows
      # stored in the database e.g. in a new column named logs

      # without num_rows the jsonData is not displayed
      rv_conversion$numRows <- length(jsonData)
      print("numRows")
      print(rv_conversion$numRows)

      mod_conv_output_viewer_server(
        "conv_mode-conv_output_viewer",
        jsonData,
        configVal,
        rv_conversion
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
      if ("id" %in% names(query)) {
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
          getPastRunResults(
            query,
            rv_general
          )
        } else {
          # TODO
          # update the run ID field with information
          # that the ID is not valid
        }
      } else {
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
    # check
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

  observeEvent(input[["landing_page-navigateToBeaconAPIs"]], {
    print("navigate to beacon api")
    updateNavbarPage(session, "nav", "beacon_api")
  })


  observeEvent(input[["landing_page-navigateToConverter"]], {
    updateNavbarPage(session, "nav", "conv")
  })

  observeEvent(input[["landing_page-navigateToPatientMode"]], {
    updateNavbarPage(session, "nav", "patient")
  })

  observeEvent(input[["landing_page-navigateToPatientMode2"]], {
    updateNavbarPage(session, "nav", "patient")
  })

  observeEvent(input[["landing_page-navigateToCohortMode"]], {
    updateNavbarPage(session, "nav", "cohort")
  })

  renderInteractiveComplexHeatmap <- function(ht_list,
                                              output_id) {
    default_opts <- list(
      close_button = FALSE,
      layout = "1|23",
      width1 = "100%",
      width2 = "700px",
      width3 = "300px",
      height1 = "500px",
      height2 = "410px"
    )

    InteractiveComplexHeatmapWidget(
      input,
      output,
      session,
      ht_list = ht_list,
      output_id = output_id,
      close_button = default_opts$close_button,
      layout = default_opts$layout,
      width1 = default_opts$width1,
      width2 = default_opts$width2,
      width3 = default_opts$width3,
      height1 = default_opts$height1,
      height2 = default_opts$height2
    )
  }

  # gets triggered by draw(ht) in mod_heatmap_server
  observeEvent(rv_patient$ht, {
    print("before rendering the patient heatmap")
    renderInteractiveComplexHeatmap(
      rv_patient$ht,
      "patient_mode-patient_heatmap"
    )
  })

  # gets triggered by draw(ht) in mod_heatmap_server
  observeEvent(rv_cohort$ht, {
    print("before rendering the cohort heatmap")
    renderInteractiveComplexHeatmap(
      rv_cohort$ht,
      "cohort_mode-cohort_heatmap"
    )
  })
}
