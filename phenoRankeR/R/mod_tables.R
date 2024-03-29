#' Table Mode Module
#'
#' @description Renders tables for the patient mode
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom DT datatable renderDT formatStyle DTOutput
#' @importFrom grDevices hcl.colors
#' @importFrom stats setNames


# TODO
# try to get rid of the dependency tibble

mod_table_phenoBlast_ui <- function(id){
  ns <- NS(id)
  card_body(
    htmlOutput(ns("phenoBlastTableHeader")),
    tags$div(
      style = "overflow-x: scroll;",

      # TODO
      # below might be a solution to have the horizontal scroll bar on the top
      # https://github.com/rstudio/DT/issues/407
      # but it does not work

      # style = HTML(
      #   "#phenoBlastTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
      # transform:rotateX(180deg);
      # }
      # #phenoBlastTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
      # transform:rotateX(180deg);
      # }"
      # ),
      DTOutput(ns("phenoBlastTable"))
    )
  )
}

mod_table_phenoRanking_ui <- function(id){
  ns <- NS(id)
  card_body(
    htmlOutput(ns("phenoRankingTableHeader")),
    DTOutput(ns("phenoRankingTable"))
  )
}

mod_table_phenoHeadsUp_ui <- function(id){
  ns <- NS(id)
  card_body(
    htmlOutput(ns("phenoHeadsUpTableHeader")),
    tags$div(
      style = "overflow-x: scroll;",
      DTOutput(ns("phenoHeadsUpTable"))
    )
  )
}

renderDefaultTable <- function(
  output, 
  tabName, 
  tableHeaderMessage, 
  colNames) {

  print("renderDefaultTable")
  print(tabName)
  print(tableHeaderMessage)
  print(colNames)

  output[[paste0(tabName, "TableHeader")]] <- renderUI({
    p(tableHeaderMessage)
  })
      
  output[[paste0(tabName, "Table")]] <- renderDT({
    datatable(
      data.frame(Value = "Placeholder"),
      options = list(
        paging = FALSE,
        info = FALSE
      ),
      colnames = colNames
    )
  })
} 

mod_table_phenoBlast_server <- function(id, 
  runId=NULL, 
  rv_patient=NULL
){
  moduleServer(id,function(input, output, session){
    ns <- session$ns

    print("in mod_table_phenoBlast_server")
    
    renderPhenoBlastTable <- function(runId) {
      
      file_path <- paste0(
        get_golem_options("patientModeOutputFolder"),
        # "data/output/rankedPatients/",
        runId,
        "/",
        runId,
        "_alignment.csv"
      )

      if (!file.exists(file_path)) {
        print("file does not exist")
        # TODO
        # throw error
        return()
      }


      blast_data <- readTxt(
        get_golem_options("patientModeOutputFolder"),
        fileName_suffix = "_alignment.csv", 
        runId = runId,
        sep = ";"
      )
      blast_data <- as.data.frame(blast_data)
      print("blast_data")
      # print(str(blast_data))

      print("nrow(blast_data)")
      print(nrow(blast_data))

      # header/first row to a character vector w/o the first column
      headers <- as.character(colnames(blast_data))[-1]
      jsonPaths <- as.character(blast_data[1, ])[-1]

      # if there is no jsonPath
      if(length(jsonPaths) == 0){
        print("jsonPaths is empty")
        # TODO
        # throw error
        return()
      }

      print("jsonPaths")
      print(jsonPaths)

      # map each jsonPath to a header
      jsonPath_to_header <- setNames(headers, jsonPaths)

      # get the unique top level keys from the jsonPaths
      # replace the top level keys with colors
      # the values of the dictionary are the headers
      # e.g. {"orange": ["Female","male"]}
      key_value_pairs <- strsplit(jsonPaths, "\\.")
      dictionary <- setNames(jsonPaths, sapply(key_value_pairs, function(x) x[1]))
      topLevels <- unique(
        sapply(strsplit(jsonPaths, "\\."), function(x) x[1])
      )

      print("topLevels")
      print(topLevels)

      # suggestion by Sofia
      # colors for the phenoblast table
      # should be in that range
      # hsla(170, 30%, 80%, 1)
      # s & l should be the fixed
      # h should be the variable (1-360)
      hex_colors <- sample(hcl.colors(length(topLevels), palette = "pastel1"))
      print("hex_colors")
      print(hex_colors)

      # TODO
      # !BUG
      # when using as reference: individuals.json
      # and as target: patient.json
      # as include-terms: geographicOrigin

      # [1] "hex_colors"
      # character(0)
      # Warning: Error in grep: invalid 'pattern' argument

      # maybe it would be a good idea to hardcode each color to
      # to a specific toplevel (for BFF it would be 11)
      # So the user gets not confused when the colors change with each re-ranking

      # replace all keys with a color and the value with the value
      # of the dictionary jsonPath_to_header
      color_scheme <- list()
      for (i in 1:length(topLevels)) {
        dict_values <- dictionary[grep(topLevels[i], names(dictionary))]

        # replace each value with the header
        for (j in 1:length(dict_values)) {
          dict_values[j] <- jsonPath_to_header[dict_values[j]]
        }
        color_scheme[[hex_colors[i]]] <- dict_values
      }

      # remove the 1st row
      blast_data <- blast_data[-1, ]

      # set the first column to ID
      colnames(blast_data)[1] <- "Id"

      col_colors <- list()
      for (color in names(color_scheme)) {
        for (col_name in color_scheme[[color]]) {
          col_colors[[col_name]] <- color
        }
      }

      # # TODO
      # # figure out how to search but keep the first row fixed
      output$phenoBlastTable <- renderDT({
        dt <- datatable(
          blast_data,
          selection = "single",
          rownames = FALSE,
          escape = FALSE,
          extensions = c("FixedColumns"),
          options = list(
            scrollY = "500px",
            scrollX = TRUE,
            fixedHeader = TRUE,
            fixedColumns = list(leftColumns = 1),
            paging = FALSE,
            searching = FALSE,
            ordering = FALSE,
            initComplete = JS(
              "function(settings, json) {",
              paste0(unlist(lapply(names(col_colors), function(col_name) {
                sprintf(
                  "$('th').filter(function() { return $(this).text() === \"%s\"; }).css('background-color', '%s');",
                  # "$('th:contains(\"%s\")').css('background-color', '%s');",
                  col_name, col_colors[[col_name]]
                )
              })), collapse = " "),
              "}"
            )
          )
        )
        # Apply color to each column based on the named list
        # for (col_name in names(col_colors)) {
        #   dt <- dt %>% formatStyle(columns = col_name, backgroundColor = col_colors[[col_name]])
        # }
        for (col_name in names(col_colors)) {
          dt <- do.call(
            "formatStyle", 
            list(
              dt, 
              columns = col_name, 
              backgroundColor = col_colors[[col_name]]
            )
          )
        }
        dt
      }, )
      output$phenoBlastTableHeader <- renderUI({
        div()
      })
      return (blast_data)
    }

    if(is.null(runId)){
      # This information would be better if it comes
      # from a config file
      renderDefaultTable(
        output,
        "phenoBlast", 
        "Click on Rank", 
        c("Id", "V1", "V2", "V3", "V4", "V5", 
          "V6", "V7", "V8", "V9", "V10")
      )
      return()
    }
    blast_data <- renderPhenoBlastTable(runId)

    observeEvent(input$phenoBlastTable_row_last_clicked, {

      # TODO
      # check for an existing patient$id
      # if there is already one when switching between
      # the tabs Binary representation and Ranking
      # Do not change the patient$id
      # and select the row in the table to which the patient$id belongs to

      row <- input$phenoBlastTable_row_last_clicked
      print("rv_patient")
      print(rv_patient)
      blast_data <- rv_patient$blastData
      ranking_df <- rv_patient$rankingDf
      print(paste("phenoBlastTable row", row, "clicked"))

      clickedRowData <- blast_data[row, ]
      patient_id <- strsplit(clickedRowData$Id, split = "\\|")[[1]][2]
      rv_patient$id <- patient_id #-> triggers the rerendering of the phenoHeadsUpTable
      #* Note: you do not seem to be able to trigger the rendering
      #* from this module because then the namespace is not correct

      mod_table_phenoHeadsUp_server(
        "phenoHeadsUpTable",
        rv_patient = rv_patient
      )
    })
    return (blast_data)
  })
}

mod_table_phenoRanking_server <- function(
  id, 
  runId=NULL, 
  rv_patient=NULL){

  moduleServer(id,function(input, output, session){
    ns <- session$ns

    renderRankingTable <- function(runId, rv_patient){
      print("Reading ranking table")

      file_path <- paste0(
        get_golem_options("patientModeOutputFolder"),
        runId,
        "/",
        runId,
        "_alignment.stdout"
      )

      if (!file.exists(file_path)) {
        print("file does not exist")
        # TODO
        # throw error
        return()
      }

      
      data <- as.matrix(
        readTxt(
          get_golem_options("patientModeOutputFolder"),
          runId=runId,
          fileName_suffix = "_alignment.stdout", 
          row_names = 1,
        )
      )
      print("Finished reading ranking table")

      df <- as.data.frame(data, stringsAsFactors = FALSE)
      ref_vs_target_targetId <- df[1, "TARGET.ID."]

      cols_to_remove <- c("TARGET.ID.", "FORMAT", "WEIGHTED")
      df <- df[, !(names(df) %in% cols_to_remove)]


      weights_file_path <- file.path(
        get_golem_options("weightsUploadFolder"),
        paste0(runId,".yaml")
      )

      weighted <- "no"
      if (file.exists(weights_file_path)) {
        weighted <- "yes"
      }

      output$phenoRankingTableHeader <- renderUI({
        p(
          paste0(
            "Input format: ",
            rv_patient$inputFormat,
            " |> Target ID: ",
            ref_vs_target_targetId,
            " |> Weighted: ",
            weighted
          )
        )
      })

      # TODO
      # check if you can make the lower table card collapsible
      colnames(df) <- c(
        "Reference(ID)", "Length",
        "Hamming Distance", "Distance Z-Score",
        "Distance P-Value", "Distance Z-Score (Rand)",
        "Jaccard Index", "Jaccard Z-Score", "Jaccard P-Value"
      )
      ranking_df <- df
      ranking_df$Rank <- as.numeric(rownames(ranking_df))
      ranking_df <- ranking_df[order(ranking_df$Rank), ]

      # ranking_df <- rownames_to_column(df, "Rank")
      # ranking_df$Rank <- as.numeric(ranking_df$Rank)

      cols_to_num <- 3:ncol(ranking_df)
      ranking_df[, cols_to_num] <- lapply(ranking_df[, cols_to_num], as.numeric)

      rv_patient$rankingDf <- ranking_df

      output$phenoRankingTable <- renderDT({
        datatable(
          ranking_df,
          rownames = FALSE,
          selection = "single",
          options = list(
            paging = FALSE,
            info = FALSE
          )
        )
      })
      return (ranking_df)
    }

    if(is.null(runId)){
      renderDefaultTable(
        output,
        "phenoRanking", 
        "Click on Rank", 
        c("Rank", "Reference(ID)", "Length", 
          "Hamming Distance", "Distance Z-Score", 
          "Distance P-Value", "Distance Z-Score (Rand)", 
          "Jaccard Index", "Jaccard Z-Score", "Jaccard P-Value"
        )
      )
      return()
    }

    ranking_df <- renderRankingTable(runId, rv_patient)
    
    observeEvent(input$phenoRankingTable_row_last_clicked, {

      # TODO
      # check for an existing patient$id
      # if there is already one when switching between
      # the tabs Binary representation and Ranking
      # Do not change the patient$id
      # and select the row in the table to which the patient$id belongs to

      row <- input$phenoRankingTable_row_last_clicked
      print(paste("phenoRankingTable row", row, "clicked"))

      clickedRowData <- ranking_df[row, ]

      # get the column information based on the column name Reference(ID)
      patient_id <- clickedRowData$`Reference(ID)`
      rv_patient$id <- patient_id #-> triggers the rerendering of the phenoHeadsUpTable
      #* Note: you do not seem to be able to trigger the rendering
      #* from this module because then the namespace is not correct
    
    })
    return (ranking_df)
  })
}

mod_table_phenoHeadsUp_server <- function(
  id, 
  rv_patient=NULL){  
  
  moduleServer(id,function(input, output, session){
    renderPhenoHeadsUpTable <- function(output, rv_patient) {

      prepareTable <- function(rv_patient){

        runId <- rv_patient$runId
        
        print("in prepareTable")
        alignment_file_path <- paste0(
          get_golem_options("patientModeOutputFolder"),
          # "data/output/rankedPatients/",
          runId,
          "/",
          runId,
          "_alignment.target.csv"
        )

        print ("alignment_file_path")
        print (alignment_file_path)

        if (!file.exists(alignment_file_path)) {
          print("file does not exist")
          # TODO
          # throw error
          return()
        }
        alignment_data <- as.matrix(
          readTxt(
            get_golem_options("patientModeOutputFolder"),
            runId = runId, 
            fileName_suffix = "_alignment.target.csv",
            sep = ";"
          )
        )
        
        # print("alignment_data")
        # print(str(alignment_data))

        alignment_df <- as.data.frame(alignment_data)
        filtered_df <- alignment_df[alignment_df[, 1] == rv_patient$id, ]

        # alignment_df <- as_tibble(alignment_data)
        # filtered_df <- alignment_df[alignment_df[, 1] == rv_patient$id, ]

        # remove the first column
        filtered_df <- filtered_df[, -1]
        colnames(filtered_df) <- c(
          "Reference", "Indicator",
          "Target", "Weight",
          "Hamming Distance", "JSON Path", "Label"
        )

        ranking_df <- rv_patient$rankingDf
        ranking_table_row <- ranking_df[ranking_df[, 2] == rv_patient$id, ]
        cumulated_hamming_distance <- ranking_table_row$`Hamming Distance`
        jaccard_index <- ranking_table_row$`Jaccard Index`

        return (list(
          df=filtered_df, 
          hamDist = cumulated_hamming_distance, 
          JacIdx =jaccard_index
        ))
      }

      renderTable <- function(output, values){
        
        print("in renderTable")

        cumulated_hamming_distance <- values$hamDist
        jaccard_index <- values$JacIdx
        filtered_df <- values$df

        # print("filtered_df")
        # print(str(filtered_df))
        
        # print("ns(phenoHeadsUpTable-phenoHeadsUpTableHeader)")
        # ns("phenoHeadsUpTable-phenoHeadsUpTableHeader")

        # TODO is not rendering the table
        # output[["patient_mode-phenoHeadsUpTable"]] <- renderUI({

        output$phenoHeadsUpTableHeader <- renderUI({
          p(
            paste0(
              "Cumulated Hamming Distance: ",
              cumulated_hamming_distance,
              " |> Jaccard Index: ",
              jaccard_index
            )
          )
        })
        output$phenoHeadsUpTable <- renderDT({
          datatable(
            filtered_df,
            rownames = FALSE,
            options = list(
              paging = FALSE,
              info = FALSE,
              scrollY = "500px",
              scrollX = TRUE
            )
          )
        })
        print("rendered table")
      }
      
      tableVals <- prepareTable(rv_patient)
      renderTable(output, tableVals)
    }

    if (is.null(rv_patient)){
      renderDefaultTable(
        output,
        "phenoHeadsUp", 
        "After ranking click on any row in the table above", 
        c("Reference", "Indicator", "Target", 
          "Weight", "Hamming Distance", 
          "JSON Path", "Label") 
      )
      return()
    }
    renderPhenoHeadsUpTable(output, rv_patient)
  })
}
