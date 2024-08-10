#' Heatmap Module Placeholder
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapWidget
#' @importFrom ComplexHeatmap pheatmap draw
#' @importFrom shiny NS htmlOutput moduleServer
#' @importFrom utils tail
#' @noRd

# The heatmap_ui below is not working
# because up to now InteractiveComplexHeatmap does not
# yet support shiny modules (see issue #105)
# https://github.com/jokergoo/InteractiveComplexHeatmap/issues/105

# There is a pull request that might fix this issue
# https://github.com/jokergoo/InteractiveComplexHeatmap/pull/107
# but it is not yet merged

mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("heatmap"))
}


make_bold_names <- function(mat, rc_names) {
  bold_names <- rownames(mat)
  ids <- match(rc_names, rownames(mat))
  for (i in ids) {
    bold_names[i] <- as.expression(bquote(bold(.(rownames(mat)[i]))))
  }
  bold_names
}

mod_heatmap_server <- function(
  id, 
  runId, 
  rv, 
  mode,
  uploaded_files_count=NULL
  ) {
  moduleServer(id,function(input, output, session){
    
    print("mod_heatmap_server")
    print("runId")
    print(runId)
    print("rv")
    print(rv)
    print("mode")
    print(mode)

    if(mode == "patient") {
      filePath <- paste0(
        get_golem_options("patientModeOutputFolder"),
        # "data/output/rankedPatients/",
        runId,
        "/",
        runId,
        ".txt"
      )

      if (!file.exists(filePath)) {
        print("file does not exist")
        # TODO
        # throw error
        return()
      }

      merged_data <- as.matrix(
        readTxt(
          get_golem_options("patientModeOutputFolder"),
          runId = runId,
          row_names = 1
        )
      )

      merged_df <- as.data.frame(merged_data)
      targetId <- tail(row.names(merged_df), 1)
      row_cols_labels <- make_bold_names(
        merged_data,
        c(targetId)
      )

      # TODO
      #!BUG
      # Warning: Error in : You should have at least two distinct break values.

      # when using as reference: individuals.json
      # and as target: patient.json
      # as include-terms: disease

      print("row_cols_labels")
      print(row_cols_labels)

      ### heatmap ###
      ht <- pheatmap(
        merged_data,
        labels_col = row_cols_labels,
        labels_row = row_cols_labels,
      )
    } else {
      filePath <- paste0(
        get_golem_options("cohortModeOutputFolder"),
        runId,
        "/",
        runId,
        ".txt"
      )

      if (!file.exists(filePath)) {
        print("file does not exist")
        # TODO
        # throw error
        return()
      }

      print("filePath")
      print(filePath)

      data <- as.matrix(
        readTxt(
          get_golem_options("cohortModeOutputFolder"),
          runId = runId,
          row_names = 1
        )
      )
      cohort_row_names <- rownames(data)

      #---- heatmap ----
      if (uploaded_files_count == 1) {
        ht <- pheatmap(data)
      } else {
        # TODO
        # for more then one file
        # the different cohort label should be colored
        ht <- pheatmap(data)

        # filter rows based on the target prefix
        # target_prefix <- "T_"
        # target_row_names <- cohort_row_names[grep(target_prefix, cohort_row_names)]
        # cohort_ht <- ComplexHeatmap::pheatmap(
        #   data,
        #   labels_row = make_bold_names(data, base::rownames, c(target_row_names)),
        #   labels_col = make_bold_names(data, base::rownames, c(target_row_names))
        # )
      }
    }

    # TODO
    # overwrite click action to filter the table below
    # https://jokergoo.github.io/InteractiveComplexHeatmap/reference/makeInteractiveComplexHeatmap.html
    rv$ht <- draw(ht) # =>triggers the observeEvent in app_server

    # below need to to be in app_server.R
    # because InteractiveComplexHeatmap does not yet support shiny_modules
    # InteractiveComplexHeatmapWidget(
    #   input,
    #   output,
    #   session,
    #   ht,
    #   output_id = "heatmap",
    #   close_button = FALSE,
    #   layout = "1|23",
    #   width1 = "100%",
    #   width2 = "700px",
    #   width3 = "300px",
    #   height1 = "500px",
    #   height2 = "410px"
    # )
  })
}
