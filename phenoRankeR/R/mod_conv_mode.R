#' Sim Mode Module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom gridlayout grid_container grid_card grid_place
#' @importFrom shinyAce aceEditor
#' @importFrom jsonlite read_json fromJSON toJSON
#' @import shiny

conv_mode_layout <- c(
  "           500px       1fr      40px",
  "30px       btn         convRes  btn_show_conv_history",
  "600px      opts        convRes  btn_show_conv_history",
  "180px      download    convRes  btn_show_conv_history",
  "1px        version     version  version              "
)

conv_mode_note <- tagList(
  tags$p("Note:"),
  tags$p("if your csv file is coming from RedCap,
    we suggest you to use the following software
    to create the BFF or PXF input files for Pheno-Ranker:")
)

convert_pheno_icon <- tags$div(
  icon("github"),
  tags$a(
    "Convert-Pheno: A software toolkit for the interconversion of standard data models for phenotypic data",
    href = "https://github.com/CNAG-Biomedical-Informatics/convert-pheno",
    target = "_blank"
  )
)

dl_explanation <- tags$p(
  "Here you can download after the conversion
  the generated JSON file and
  the configuration file for Pheno-Ranker"
)

mod_conv_mode_ui <- function(id) {
  ns <- NS(id)
  version <- get_golem_options("packageVersion")

  grid_container(
    layout = conv_mode_layout,
    grid_place(
      area = "btn",
      actionButton(
        ns("startConversion"),
        "Convert",
        class = "btn btn-primary"
      )
    ),
    grid_card(
      area = "opts",
      card_header("Options"),
      card_body(
        mod_loader_ui(ns("loader_conv")),
        fileInput(
          ns("csv"),
          "Upload a csv file",
          multiple = FALSE,
          accept = c(
            ".csv"
          )
        ),
        downloadButton(ns("downloadExampleBtn"), "Download Example CSV"),
        textInput(ns("primaryKey"), "primary-key", value = "id"),
        textInput(ns("delimiter"), "delimiter", value = ";"),
        conv_mode_note,
        convert_pheno_icon
      )
    ),
    grid_card(
      area = "convRes",
      card_header("Conversion Results"),
      full_screen = TRUE,
      card_body(
        verbatimTextOutput("conversionId"),
        mod_conv_output_viewer_ui(ns("conv_output_viewer"))
      )
    ),
    grid_card(
      area = "download",
      card_header = "Download Conversion results",
      card_body(
        grid_container(
          layout = c(
            "      1fr          1fr          1fr          ",
            "75px  explanation  explanation  explanation  ",
            "25px  output       cfg          both         "
          ),
          gap_size = "5px",
          grid_place(
            area = "explanation",
            div(
              dl_explanation
            )
          ),
          grid_place(
            area = "output",
            downloadButton(ns("output"), "output")
          ),
          grid_place(
            area = "cfg",
            downloadButton(ns("cfg"), "config")
          ),
          grid_place(
            area = "both",
            downloadButton(ns("both"), "both")
          )
        )
      )
    ),
    grid_place(
      area = "btn_show_conv_history",
      mod_show_history_button_ui(ns("ConvertHistorySidebar"))
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

mod_conv_output_viewer_ui <- function(id) {
  ns <- NS(id)
  grid_container(
    layout = c(
      "       1fr         1fr         ",
      "600px  json_viewer yaml_viewer "
    ),
    gap_size = "10px",
    grid_place(
      area = "json_viewer",
      mod_json_viewer_ui(ns("json_viewer_conv_mode"))
    ),
    grid_place(
      area = "yaml_viewer",
      uiOutput(ns("yaml_viewer"))
    )
  )
}

mod_conv_output_viewer_server <- function(id, conv_out, cfg_out, rv_conversion) {
  moduleServer(id, function(input, output, session) {

    print("cfg_out")
    print(cfg_out)
    output$yaml_viewer <- renderUI({
      div(
        card_header("Config file"),
        aceEditor(
          outputId = "csv_conversion_config_file",
          value = cfg_out,
          mode = "yaml",
          theme = "github",
          height = "75vh",
          readOnly = TRUE,
          fontSize = 20
        )
      )
    })

    observeEvent(rv_conversion$numRows, {
      print("observeEvent rv_conversion$numRows")
      print(rv_conversion$numRows)

      mod_json_viewer_server(
        "json_viewer_conv_mode",
        checkboxes = NULL,
        bff_out = NULL,
        pxf_out = NULL,
        arraySizeInput = rv_conversion$numRows,
        conv_out = conv_out
      )
    })
  })
}

mod_conv_mode_server <- function(id, session, db_conn, rv_conversion, rv_general) {
  # NOTE somehow this function is only working with the
  # namespace defined here
  ns <- session$ns
  moduleServer(id, function(input, output, session) {
    submit_clicked <- reactive({
      input$startConversion
    })

    mod_loader_server(
      ns("loader_conv"),
      session,
      "startConversion",
      submit_clicked,
      "Conversion in progress",
      "Please wait while the conversion is ongoing...",
      rv_conversion$numRows
    )

    mod_show_history_button_server(
      "ConvertHistorySidebar",
      "conv",
      "ConvertHistorySidebar",
      db_conn,
      rv_general$user_email
    )

    # below is not working when running inside the docker container
    output$downloadExampleBtn <- downloadHandler(
      filename = "example.csv",
      content = function(file) {
        file.copy(
          normalizePath("inst/extdata/examples/example.csv"),
          file
        )
      }
    )

    conversionOutputFolder <- get_golem_options(
      "conversionOutputFolder"
    )

    observeEvent(input$csv, {
      print("input$csv")
      print(input$csv)
      req(input$csv)

      cmd <- paste(
        "wc -l",
        input$csv$datapath
      )

      cmd_out <- system(
        cmd,
        intern = TRUE
      )

      num_rows <- as.numeric(
        strsplit(cmd_out, " ")[[1]][1]
      ) - 1

      print("num_rows")
      print(num_rows)

      rv_conversion$numRows <- num_rows
    })

    # TODO
    # put in a more general function
    # because it is used in mod_sim_mode as well
    observe({
      req(rv_conversion$id)
      print("rv_conversion$id")
      print(rv_conversion$id)

      path <- paste0(
        rv_general$user_dirs$output$conv,
        "/",
        rv_conversion$id
      )

      conv_output_fn <- file.path(
        path,
        paste0(rv_conversion$id, ".json")
      )

      config_fn <- file.path(
        path,
        paste0(rv_conversion$id, "_config.yaml")
      )

      print("conv_output_fn")
      print(conv_output_fn)

      print("config_fn")
      print(config_fn)

      output$output <- outputDownloadHandler(
        conv_output_fn,
        "conversionOutput"
      )

      output$cfg <- outputDownloadHandler(
        config_fn,
        "conversionConfig"
      )

      output$both <- outputDownloadHandler(
        list(conv_output_fn, config_fn),
        list("conversionOutput", "conversionConfig"),
        output_name = "phenoRankerConv.zip",
        zip_download = TRUE
      )
    })

    output$output <- outputDownloadHandler(
      rv_conversion$outputJson,
      "conversionOutput"
    )

    output$cfg <- outputDownloadHandler(
      rv_conversion$configYaml,
      "conversionConfig"
    )

    output$both <- outputDownloadHandler(
      list(rv_conversion$outputJson, rv_conversion$configYaml),
      list("conversionOutput", "conversionConfig"),
      zip_download = TRUE
    )

    observeEvent(input$convertBtnClicked, {
      # TODO
      # check if the delimiter field is valid

      convSettings <- c(
        primary_key = input$primaryKey,
        delimiter = input$delimiter
      )

      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      output$conversionId <- renderText(paste0("RUN ID: ", timestamp))

      session$sendCustomMessage(
        type = "changeURL",
        message = list(mode = "conv", id = timestamp)
      )

      csvConvBin <- get_golem_options("PHENO_CSV_CONV_BIN")
      # outputFolder <- get_golem_options("conversionOutputFolder")

      outputFolder <- rv_general$user_dirs$output$conv

      print("input$csv$datapath")
      print(input$csv$datapath)

      # get the number of rows in the csv file
      # TODO
      # do not do this after click but after having uploaded a file
      # cmd <- paste(
      #   "wc -l",
      #   input$csv$datapath
      # )

      # cmd_out <- system(
      #   cmd,
      #   intern = TRUE
      # )

      # num_rows <- as.numeric(
      #   strsplit(cmd_out, " ")[[1]][1]
      # ) - 1

      # print("num_rows")
      # print(num_rows)

      # rv_conversion$numRows <- num_rows

      # create folder for the conversion output
      dir.create(file.path(outputFolder, timestamp))

      # copy the uploaded file to the conversion output folder
      new_path <- file.path(
        outputFolder,
        timestamp,
        paste0(timestamp, ".csv")
      )

      file.copy(
        input$csv$datapath,
        new_path
      )

      cmd <- paste(
        csvConvBin,
        "--input",
        new_path
      )

      print("primary key:")
      print(input$primaryKey)
      print("delimiter:")
      print(input$delimiter)

      # read the first line of the csv file
      # to get the column names
      firstLine <- readLines(
        new_path,
        n = 1
      )

      line_split <- strsplit(firstLine, input$delimiter)
      if (!(input$primaryKey %in% gsub("\"", "", line_split[[1]]))) {
        print("primary key not in data")
        print("line_split[[1]][1:5]")
        print(line_split[[1]][1:5])
        cmd <- paste(
          cmd,
          "--generate-primary-key"
        )
      }

      cmd <- paste(
        cmd,
        "--primary-key",
        input$primaryKey,
        "--separator",
        paste0(
          "'",
          input$delimiter,
          "'"
        )
      )

      script_status <- system(cmd, intern = TRUE)
      if (length(script_status) > 0) {
        # showNotification(script_status, type = "error")
        stop("Perl script failed")
      }

      label <- paste0("file: ", input$csv$name)

      # TODO
      # it would be nice to have the used primary key in the label

      settings <- list()
      store_job_in_db(
        timestamp,
        rv_general$user_email,
        "conv",
        label,
        settings,
        db_conn
      )

      # query <- sprintf(
      #   "
      #     INSERT INTO jobs (run_id, user_id, mode, label, settings, status)
      #     VALUES (%s,%s,'%s','%s',cast('%s' as JSONB),'%s')
      #   ",
      #   timestamp, 1, "conv", label, toJSON(settings), "success"
      # )
      # dbExecute(db_conn, query)

      # rerender the history sidebar
      click("ConvertHistorySidebar-btn_show_history")

      # show the converted file
      jsonData <- read_json(
        file.path(
          outputFolder,
          timestamp,
          paste0(timestamp, ".json")
        )
      )

      # print(jsonData)

      # value <- paste(
      #   readLines(
      #     file.path(
      #       outputFolder,
      #       timestamp,
      #       paste0(timestamp,"_config.yaml")
      #     )
      #   ),
      #   collapse = "\n"
      # )

      # not sure if the yaml library is needed?
      configOut <- yaml.load_file(
        file.path(
          outputFolder,
          timestamp,
          paste0(timestamp, "_config.yaml")
        )
      )

      print("executing mod_conv_output_viewer_server")
      print("rv_conversion")
      print(rv_conversion)

      mod_conv_output_viewer_server(
        "conv_output_viewer",
        jsonData,
        configOut,
        rv_conversion
      )
    })
  })
}
