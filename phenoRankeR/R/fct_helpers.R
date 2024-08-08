#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'

### Data loaders ###

#' Get file extension
#' @noRd

get_file_ext <- function(filepath) {
  ext <- sub(".*\\.", "", filepath)
  if (ext == filepath) {
    return(NULL) # Return NULL if there is no file extension
  }
  return(ext)
}

#' Read Tabular data
#' @importFrom utils read.table
#' @noRd

readTxt <- function(
    path,
    fileName_suffix = NULL,
    runId = NULL,
    sep = "",
    header = TRUE,
    row_names = NULL) {
  # examples for path
  # data/output/rankedPatients/
  # data/output/rankedCohortMatrixes/

  if (Sys.getenv("R_PACKAGE_BUILD") == "T") {
    if ("rankedPatients" %in% path) {
      path <- "tests/fixtures/rankedPatients"
      runId <- "rankedPatients"
    } else if ("rankedCohortMatrixes" %in% path) {
      path <- "tests/fixtures/rankedCohortMatrixes"
      runId <- "rankedCohortMatrixes"
    }
  }

  suffix <- ".txt"
  if (!is.null(fileName_suffix)) {
    suffix <- fileName_suffix
  }

  filePath <- paste0(
    path,
    "/",
    runId,
    "/",
    runId,
    suffix
  )

  if (!file.exists(filePath)) {
    print("file does not exist")
    # TODO
    # throw error
    # or during build return a default
    return()
  }

  txtData <- tryCatch(
    {
      read.table(
        filePath,
        header = header,
        row.names = row_names,
        sep = sep
      )
    },
    error = function(e) {
      print("Error reading the file: ")
      cat("Error reading the file: ", filePath, "\n")
      print(e)
      # throw error
      return()
    }
  )
  return(txtData)
}


generateJsonView <- function(jsonOutput, title, width = 12) {
  # TODO
  # figure out how to give the div the heigh 85vh
  print("generateJsonView")

  print("jsonOutput")
  print(jsonOutput)

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


### Validators ###

#' Validate YAML input
#' @import yaml
#' @importFrom jsonlite toJSON
#' @noRd

validateYAML <- function(yaml_input) {
  if (yaml_input == "") {
    return("No YAML input")
  }
  result <- tryCatch(
    {
      yaml.load(yaml_input)
      "YAML is valid"
    },
    error = function(e) {
      print(as.character(e))
      error_message <- as.character(e)
      match <- regexpr("(?<=error: ).*", error_message, perl = TRUE)
      ifelse(match > 0, substr(error_message, match, match + attr(match, "match.length") - 1), NA)
      # str_extract(as.character(e), "(?<=error: ).*")
    }
  )
  return(result)
}

#' Data download handler'
#' @importFrom shiny downloadHandler
#' @import zip
#' @noRd

outputDownloadHandler <- function(
    data_sources, file_names, mode, output_name = "", zip_download = FALSE) {
  downloadHandler(
    filename = function() {
      ext <- ".json"

      if (length(file_names) == 1 && grepl("conversionConfig", file_names)) {
        ext <- ".yaml"
      }

      if (zip_download) {
        paste0(output_name)
      } else {
        paste0(file_names, ext)
      }
    },
    content = function(file) {
      if (zip_download) {
        temp_dir <- tempdir()
        print("temp_dir")
        print(temp_dir)

        # Initialize a vector to hold the full filenames with extensions
        full_file_names <- c()
        for (i in seq_along(data_sources)) {
          # check if the file is a yaml file
          fn <- file_names[[i]]
          print("fn")
          print(fn)

          ext <- ".json"
          if (fn == "conversionConfig") {
            ext <- ".yaml"
          }

          new_file_name <- paste0(file_names[i], ext)
          temp_file <- file.path(temp_dir, new_file_name)
          file.copy(
            normalizePath(data_sources[[i]]),
            temp_file
          )

          # Add the json file to the vector
          full_file_names <- c(full_file_names, new_file_name)
          print("full_file_names")
          print(full_file_names)

          # if (fn == "conversionConfig") {
          #   print("inside conversionConfig")
          #   content_file <- file.path(paste0(fn, ".yaml"))
          #   print("content_file")
          #   print(content_file)
          #   writeLines(data_sources[[i]], content_file)

          #   # Add the yaml file to the vector
          #   full_file_names <- c(full_file_names, paste0(fn, ".yaml"))
          # } else {
          #   # content_file <- file.path(paste0(file_names[i], ".json"))
          #   # writeLines(as.character(toJSON(data_sources[[i]])), content_file)
          #   print("data_sources[[i]]")
          #   print(data_sources[[i]])

          #   print("normalizePath(data_sources[[i]])")
          #   print(normalizePath(data_sources[[i]]))

          #   print("file.path(temp_dir, file_names[i])")
          #   print(file.path(temp_dir, file_names[i]))

          #   new_file_name <- paste0(file_names[i], ".json")
          #   temp_file <- file.path(temp_dir, new_file_name)
          #   file.copy(
          #     normalizePath(data_sources[[i]]),
          #     temp_file
          #   )

          #   # Add the json file to the vector
          #   full_file_names <- c(full_file_names, new_file_name)
          #   print("full_file_names")
          #   print(full_file_names)
          # }
        }
        setwd(temp_dir)
        zip(
          file,
          files = full_file_names
        )
        on.exit(unlink(temp_dir))
      } else {
        file_path <- normalizePath(data_sources)
        file.copy(file_path, file)

        # if (grepl("conversionConfig", file_names)) {
        #   # writeLines(data_sources, file)
        #   file_path <- normalizePath(data_sources)
        #   print("file_path")
        #   file.copy(file_path, file)
        # } else {
        #   print("data_sources")
        #   print(data_sources)
        #   file_path <- normalizePath(data_sources)
        #   print("file_path")
        #   print(file_path)
        #   file.copy(file_path, file)
        # }
      }
    }
  )
}

#' Database helper'
#' @noRd

store_job_in_db <- function(runId, userId, mode, label, settings, db_conn) {
  query <- sprintf(
    "
      INSERT INTO jobs (run_id, user_id, mode, label, settings, status)
      VALUES (%s,%s,'%s','%s',cast('%s' as JSONB),'%s')
    ",
    runId, userId, mode, label, toJSON(settings), "success"
  )
  tryCatch(
    {
      rows_affected <- dbExecute(db_conn, query)
      print("Rows affected:")
      print(rows_affected)
    },
    error = function(e) {
      print("Error occurred:")
      print(e)
    }
  )
}


### Listeners for the patient/cohort mode ###

#' Observe tab change events
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom DBI dbGetQuery
#' @noRd

observeTabChangeEvent <- function(
    input,
    session,
    panel_id,
    condition,
    conditionCheckFun,
    notificationText,
    updateValue) {
  observeEvent(input[[panel_id]], {
    if (input[[panel_id]] == condition) {
      if (conditionCheckFun()) {
        showNotification(
          notificationText,
          type = "error"
        )
        updateTabsetPanel(
          session,
          panel_id,
          updateValue
        )
      }
    }
  })
}

observeTabChangeToExampleData <- function(
    input,
    session,
    db_conn,
    panel_id,
    dropdown_id) {
  # should not be hardcoded
  user_id <- 1

  query <- sprintf(
    "SELECT run_id, label FROM jobs WHERE user_id = %d AND mode = 'input_examples' AND status = 'success' ORDER BY submitted_at DESC",
    user_id
  )

  res <- dbGetQuery(db_conn, query)
  choices <- setNames(res$run_id, res$label)

  updateSelectInput(
    session,
    dropdown_id,
    choices = choices,
    selected = NULL
  )

  observeTabChangeEvent(
    input,
    session,
    panel_id,
    "Example data",
    function() {
      nrow(res) == 0
    },
    "Please run example data first",
    "Upload"
  )
}


observeTabChangeToSimulateData <- function(
    input,
    session,
    db_conn,
    panel_id,
    dropdown_id) {
  # should not be hardcoded
  user_id <- 1

  query <- sprintf(
    "SELECT run_id, label FROM jobs WHERE user_id = %d AND mode = 'sim' AND status = 'success' ORDER BY submitted_at DESC",
    user_id
  )

  res <- dbGetQuery(db_conn, query)
  choices <- setNames(res$run_id, res$label)

  updateSelectInput(
    session,
    dropdown_id,
    choices = choices,
    selected = NULL
  )

  observeTabChangeEvent(
    input,
    session,
    panel_id,
    "Simulated data",
    function() {
      nrow(res) == 0
    },
    "Please run simulate BFF/PXF first",
    "Upload"
  )
}

observeTabChangeToConvertedData <- function(
    input,
    session,
    db_conn,
    panel_id,
    dropdown_id) {
  # TODO
  # get user id from the database
  user_id <- 1

  query <- sprintf(
    "SELECT run_id, label FROM jobs WHERE user_id = %d AND mode = 'conv' AND status = 'success' ORDER BY submitted_at DESC",
    user_id
  )
  res <- dbGetQuery(db_conn, query)
  choices <- setNames(res$run_id, res$label)

  updateSelectInput(
    session,
    dropdown_id,
    choices = choices,
    selected = NULL
  )

  observeTabChangeEvent(
    input,
    session,
    panel_id,
    "Converted data",
    function() {
      nrow(res) == 0
    },
    "Please run CSV Conversion first",
    "Upload"
  )
}

create_new_mapping_df <- function() {
  return(data.frame(
    original_fn = character(),
    new_fn = character(),
    id_prefixes = character(),
    simulatedData = logical(),
    file_info = character(),
    stringsAsFactors = FALSE
  ))
}

#' Observe tab change events
#' @importFrom shiny observeEvent
#' @importFrom DBI dbGetQuery
#' @noRd

# The function below is not doing what it is supposed to do

observeSimulatedDataChange <- function(
    session,
    input,
    output,
    db_conn,
    rv,
    rv_sim,
    input_id,
    yaml_editor_id,
    expected_row_count) {
  # possible values:
  # input_id <-
  # "patient_sim_reference",
  # "patient_sim_target",
  # "patient_sim_cohort"

  observeEvent(input[[input_id]], {
    if (length(input[[input_id]]) == 0) {
      rv$inputFormat <- NULL
      return()
    }

    if (is.null(rv$mappingDf)) {
      mapping_df <- create_new_mapping_df()
    } else {
      mapping_df <- rv$mappingDf
    }

    file_info <- "Cohort"
    if (grepl("reference", input_id)) {
      id_prefix <- "R"
      file_info <- "Reference"
      rv$useSimulatedReference <- TRUE
      rv$useExampleReference <- FALSE
      rv$useConvertedReference <- FALSE
      mapping_df <- create_new_mapping_df()
    } else if (grepl("target", input_id)) {
      id_prefix <- "T"
      file_info <- "Target"
      rv$useSimulatedTarget <- TRUE
      rv$useExampleTarget <- FALSE
      rv$useConvertedTarget <- FALSE
      print("mapping_df before subset")
      print(mapping_df)
      mapping_df <- subset(mapping_df, file_info != "Target")
      print("mapping_df after subset")
      print(mapping_df)
    } else {
      id_prefix <- "C"
      mapping_df <- create_new_mapping_df()
    }

    if (is.null(rv$inputFormat)) {
      rv$inputFormat <- "bff"
    }

    # get the possible radio button choices from the database
    if (length(input[[input_id]]) == 1) {
      query <- sprintf(
        "
          SELECT settings FROM jobs
          WHERE run_id = '%s'
            AND user_id = %d
            AND mode = 'sim'
            AND status = 'success'
          GROUP BY settings
          HAVING COUNT(*) = %d
        ",
        input[[input_id]], 1, expected_row_count
      )
    } else {
      query <- sprintf(
        "
          SELECT settings FROM jobs
          WHERE run_id IN (%s)
            AND user_id = %d
            AND mode = 'sim'
            AND status = 'success'
        ",
        paste(input[[input_id]], collapse = ","),
        1
      )
    }
    print("query")
    print(query)

    res <- dbGetQuery(db_conn, query)
    print("res")
    print(res)


    if (expected_row_count == 1) {
      # res is a json get the values of the key outputFormats
      formats <- fromJSON(res$settings[1])$outputFormats
      # print(formats)
    } else {
      output_formats <- lapply(res$settings, function(setting) {
        parsed_setting <- fromJSON(setting)
        return(parsed_setting$outputFormats)
      })
      formats <- Reduce(intersect, output_formats)
      if (length(formats) == 1) {
        formats <- list(formats)
      }
    }
    print("formats")
    print(formats)

    # selected should be the first element of the list
    selected <- formats[1]

    print("session$ns(simulatedRefsInputFormatRadio)")
    print(session$ns("simulatedRefsInputFormatRadio"))

    output$simulatedRefsInputFormats <- renderUI({
      radioButtons(
        session$ns("simulatedRefsInputFormatRadio"),
        "Format:",
        choices = formats,
        selected = selected
      )
    })

    # TODO
    # render below only if in cohort tab
    output$simulatedCohortInputFormats <- renderUI({
      radioButtons(
        session$ns("simulatedCohortInputFormatRadio"),
        "Format:",
        choices = formats,
        selected = selected
      )
    })

    rv_sim$simulationId <- input[[input_id]]

    print("after inputFormat")
    print(input[[input_id]])

    # !BUG
    # switching the target overwrites the whole
    # mapping_df to both values having the prefix: T
    # e.g.
    # 20230804144608.bff.json:T
    # 20230804143923.bff.json:T

    # simulatedData_input_dir <- "./data/output/simulatedData/"
    simulatedData_input_dir <- get_golem_options("simulationOutputFolder")

    rows <- lapply(1:expected_row_count, function(i) {
      id_prefix_new <- paste0(id_prefix, i)

      # Use ifelse to handle the different cases for simulationId
      simulationId <- ifelse(expected_row_count == 1, rv_sim$simulationId, rv_sim$simulationId[i])

      print("expected_row_count")
      print(expected_row_count)

      print("rv_sim$simulationId")
      print(simulationId)

      row <- data.frame(
        file_info = file_info,
        original_fn = paste0(
          simulationId,
          ".",
          rv$inputFormat,
          ".json"
        ),
        new_fn = normalizePath(
          paste0(
            simulatedData_input_dir,
            simulationId,
            ".",
            rv$inputFormat,
            ".json"
          )
        ),
        id_prefixes = id_prefix_new,
        simulatedData = TRUE,
        stringsAsFactors = FALSE
      )
      return(row)
    })

    # Create a list of rows using lapply
    # if (expected_row_count == 1) {
    #   rows <- lapply(1:expected_row_count, function(i) {
    #     id_prefix_new <- paste0(id_prefix, i)

    #     print("rv_sim$simulationId")
    #     print(rv_sim$simulationId)

    #     row <- data.frame(
    #       file_info = file_info,
    #       original_fn = paste0(
    #         rv_sim$simulationId,
    #         ".",
    #         rv$inputFormat,
    #         ".json"
    #       ),
    #       new_fn = normalizePath(
    #         paste0(
    #           simulatedData_input_dir,
    #           rv_sim$simulationId,
    #           ".",
    #           rv$inputFormat,
    #           ".json"
    #         )
    #       ),
    #       id_prefixes = id_prefix_new,
    #       simulatedData = TRUE,
    #       stringsAsFactors = FALSE
    #     )
    #     return(row)
    #   })
    # } else {
    #   rows <- lapply(1:expected_row_count, function(i) {
    #     id_prefix_new <- paste0(id_prefix, i)

    #     print("rv_sim$simulationId")
    #     print(rv_sim$simulationId[i])

    #     row <- data.frame(
    #       file_info = file_info,
    #       original_fn = paste0(
    #         rv_sim$simulationId[i],
    #         ".",
    #         rv$inputFormat,
    #         ".json"
    #       ),
    #       new_fn = normalizePath(
    #         paste0(
    #           simulatedData_input_dir,
    #           rv_sim$simulationId[i],
    #           ".",
    #           rv$inputFormat,
    #           ".json"
    #         )
    #       ),
    #       id_prefixes = id_prefix_new,
    #       simulatedData = TRUE,
    #       stringsAsFactors = FALSE
    #     )
    #     return(row)
    #   })
    # }

    # Bind rows into data frame
    rows_df <- do.call(rbind, rows)
    print("rows_df")
    print(rows_df)
    rv$mappingDf <- rbind(mapping_df, rows_df)

    print("in observeSimulatedDataChange in fct_helpers.R")
    print("rv$mappingDf")
    print(rv$mappingDf)

    # TODO
    # the function above unfortunately returns the following:

    #       [[1]]
    #   file_info             original_fn
    # 1    Cohort 20230804200724.bff.json
    # 2    Cohort 20230804182808.bff.json
    #                                                                                                              new_fn
    # 1 /home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/shiny-app/data/output/simulatedData/20230804200724.bff.json
    # 2 /home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/shiny-app/data/output/simulatedData/20230804182808.bff.json
    #   id_prefixes simulatedData
    # 1          C1          TRUE
    # 2          C1          TRUE

    # [[2]]
    #   file_info             original_fn
    # 1    Cohort 20230804200724.bff.json
    # 2    Cohort 20230804182808.bff.json
    #                                                                                                              new_fn
    # 1 /home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/shiny-app/data/output/simulatedData/20230804200724.bff.json
    # 2 /home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/shiny-app/data/output/simulatedData/20230804182808.bff.json
    #   id_prefixes simulatedData
    # 1          C2          TRUE
    # 2          C2          TRUE

    # expected would be:
    #   file_info             original_fn
    # 1    Cohort 20230804200724.bff.json
    # 2    Cohort 20230804182808.bff.json
    #                                                                                                              new_fn
    # 1 /home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/shiny-app/data/output/simulatedData/20230804200724.bff.json
    # 2 /home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/shiny-app/data/output/simulatedData/20230804182808.bff.json
    #   id_prefixes simulatedData
    # 1          C1          TRUE
    # 2          C2          TRUE

    # CHATGPT:
    # Your output is being duplicated because
    # file_info, rv_sim$simulationId, and rv$inputFormat are all vectors.
    # This is causing the data.frame function to create a data frame with rows
    # for each element of these vectors for each iteration of the loop.

    # To fix this, you should iterate over the length of one of these vectors
    # (e.g., file_info) instead of expected_row_count,
    # and then generate a single row for each iteration

    # put this into a general function
    editor_val <- ""
    for (j in 1:nrow(rv$mappingDf)) {
      print("j")
      print(j)
      editor_val <- paste0(
        editor_val,
        rv$mappingDf$original_fn[j],
        ":",
        rv$mappingDf$id_prefixes[j],
        "\n"
      )
      print("editor_val")
      print(editor_val)
    }
    updateAceEditor(
      session,
      yaml_editor_id,
      value = editor_val
    )
  })
}

observeExampleDataChange <- function(
    session,
    input,
    output,
    rv,
    rv_input_examples,
    input_id,
    yaml_editor_id,
    expected_row_count) {
  # possible values:
  # input_id <-
  # "patient_example"

  observeEvent(input[[input_id]], {
    rv_input_examples$retrievalId <- input[[input_id]]

    # if (input[[input_id]] == "") {
    #   rv$inputFormat <- NULL
    #   return()
    # }

    if (is.null(rv$mappingDf)) {
      mapping_df <- create_new_mapping_df()
    } else {
      mapping_df <- rv$mappingDf
    }

    file_info <- "Cohort"
    if (grepl("reference", input_id)) {
      id_prefix <- "R"
      file_info <- "Reference"
      rv$useExampleReference <- TRUE
      rv$useSimulatedReference <- FALSE
      rv$useConvertedReference <- FALSE
      mapping_df <- create_new_mapping_df()
    } else if (grepl("target", input_id)) {
      id_prefix <- "T"
      file_info <- "Target"
      rv$useExampleTarget <- TRUE
      rv$useSimulatedTarget <- FALSE
      rv$useConvertedTarget <- FALSE
      print("mapping_df before subset")
      print(mapping_df)
      mapping_df <- subset(mapping_df, file_info != "Target")
      print("mapping_df after subset")
      print(mapping_df)
    } else {
      id_prefix <- "C"
      mapping_df <- create_new_mapping_df()
    }

    exampleDataInputDir <- get_golem_options("inputExamplesOutputFolder")

    rows <- lapply(1:expected_row_count, function(i) {
      id_prefix_new <- paste0(id_prefix, i)

      # Use ifelse to handle the different cases for simulationId
      retrievalId <- ifelse(expected_row_count == 1, rv_input_examples$retrievalId, rv_input_examples$retrievalId[i])

      print("expected_row_count")
      print(expected_row_count)

      print(" rv_input_examples$retrievalId ")
      print(retrievalId)

      row <- data.frame(
        file_info = file_info,
        original_fn = paste0(
          retrievalId,
          ".",
          rv$inputFormat,
          ".json"
        ),
        new_fn = normalizePath(
          paste0(
            exampleDataInputDir,
            retrievalId,
            ".",
            rv$inputFormat,
            ".json"
          )
        ),
        id_prefixes = id_prefix_new,
        simulatedData = TRUE,
        stringsAsFactors = FALSE
      )
      return(row)
    })

    rows_df <- do.call(rbind, rows)
    print("rows_df")
    print(rows_df)
    rv$mappingDf <- rbind(mapping_df, rows_df)


    # row <- data.frame(
    #   file_info = file_info,
    #   original_fn = paste0(
    #     input[[input_id]],
    #     ".pxf.json"
    #   ),
    #   new_fn = normalizePath(
    #     paste0(
    #       exampleDataInputDir,
    #       input[[input_id]],
    #       ".pxf.json"
    #     )
    #   ),
    #   id_prefixes = id_prefix,
    #   simulatedData = FALSE,
    #   stringsAsFactors = FALSE
    # )

    # print("row")
    # print(row)

    # rv$mappingDf <- rbind(mapping_df, row)

    # put this into a general function
    editor_val <- ""
    for (i in 1:nrow(rv$mappingDf)) {
      editor_val <- paste0(
        editor_val,
        rv$mappingDf$original_fn[i],
        ":",
        rv$mappingDf$id_prefixes[i],
        "\n"
      )
    }
    updateAceEditor(
      session,
      yaml_editor_id,
      value = editor_val
    )
  })
}


observeConvertedDataChange <- function(
    session,
    input,
    output,
    rv,
    rv_conversion,
    input_id,
    yaml_editor_id,
    yaml_cfg_editor_id) {
  # possible values:
  # input_id <-
  # "patient_conv_reference"

  print("observeConvertedDataChange")
  observeEvent(input[[input_id]], {
    convertedId <- input[[input_id]]
    rv_conversion$id <- convertedId
    print(convertedId)

    if (convertedId == "") {
      rv$inputFormat <- NULL
      return()
    }

    if (is.null(rv$mappingDf)) {
      mapping_df <- create_new_mapping_df()
    } else {
      mapping_df <- rv$mappingDf
    }

    file_info <- "Cohort"
    if (grepl("reference", input_id)) {
      id_prefix <- "R"
      file_info <- "Reference"
      rv$useExampleReference <- FALSE
      rv$useSimulatedReference <- FALSE
      rv$useConvertedReference <- TRUE

      mapping_df <- create_new_mapping_df()
    } else if (grepl("target", input_id)) {
      id_prefix <- "T"
      file_info <- "Target"
      rv$useExampleTarget <- FALSE
      rv$useSimulatedTarget <- FALSE
      rv$useConvertedTarget <- TRUE
      print("mapping_df before subset")
      print(mapping_df)
      mapping_df <- subset(mapping_df, file_info != "Target")
      print("mapping_df after subset")
      print(mapping_df)
    } else {
      id_prefix <- "C"
      mapping_df <- create_new_mapping_df()
    }

    convertedDataInputDir <- get_golem_options("conversionOutputFolder")
    print("convertedDataInputDir")
    print(convertedDataInputDir)

    row <- data.frame(
      file_info = file_info,
      original_fn = paste0(
        convertedId,
        ".json"
      ),
      new_fn = normalizePath(
        paste0(
          convertedDataInputDir,
          paste0(convertedId, "/"),
          convertedId,
          ".json"
        )
      ),
      id_prefixes = id_prefix,
      simulatedData = FALSE,
      stringsAsFactors = FALSE
    )
    rv$mappingDf <- rbind(mapping_df, row)

    print("rv$mappingDf")
    print(rbind(mapping_df, row))

    # put this into a general function
    editor_val <- ""
    for (i in 1:nrow(rv$mappingDf)) {
      editor_val <- paste0(
        editor_val,
        rv$mappingDf$original_fn[i],
        ":",
        rv$mappingDf$id_prefixes[i],
        "\n"
      )
    }
    updateAceEditor(
      session,
      yaml_editor_id,
      value = editor_val
    )

    # read the config file
    yamlCfg <- readLines(
      paste0(
        get_golem_options("conversionOutputFolder"),
        # "./data/output/convertedData/",
        convertedId,
        "/",
        convertedId,
        "_config.yaml"
      ),
    )

    # update the extra config
    updateAceEditor(
      session,
      yaml_cfg_editor_id,
      value = paste(yamlCfg, collapse = "\n")
    )
  })
}
