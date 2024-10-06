#' Table Mode Module
#'
#' @description Renders tables for the patient mode
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS need validate
#' @importFrom plotly plotlyOutput renderPlotly ggplotly event_register event_data
#' @importFrom ggplot2 ggplot aes geom_point labs theme element_text scale_color_discrete
#' @importFrom stats cmdscale
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%

# to prevent the linting error
globalVariables(".data")

mod_plot_mds_ui <- function(id) {
  ns <- NS(id)
  card_body(
    plotlyOutput(
      outputId = ns("plot_mds"),
      height = "600px"
    ),
    p("Mouse over or select the points to see the patient IDs"),
    verbatimTextOutput(ns("selected_points"))
  )
}

renderPlots <- function(
    runId,
    rv,
    rv_general,
    mode,
    uploaded_files_count = NULL) {
  if (mode == "patient") {
    filePath <- paste0(
      rv_general$user_dirs$output$pats_ranked,
      # get_golem_options("patientModeOutputFolder"),
      "/",
      runId,
      "/",
      runId,
      ".txt"
    )

    if (!file.exists(filePath)) {
      print("file does not exist")
      return()
    }

    merged_data <- as.matrix(
      readTxt(
        rv_general$user_dirs$output$pats_ranked,
        runId = runId,
        row_names = 1
      )
    )

    print("try to draw patient MDS scatter plot")
    # d <- dist(merged_data)

    # TODO
    # !BUG cmdscale runs into
    # Error in cmdscale: 'k' must be in {1, 2, ..  n - 1}
    # when running it with converted patient data (the example data from pheno-ranker)

    # TODO
    # !BUG cmdscale runs into
    # only 0 of the first 2 eigenvalues are > 0
    # Warning: Error in [: subscript out of bounds

    # when using as reference: individuals.json
    # and as target: patient.json
    # as include-terms: disease

    fit <- cmdscale(merged_data, eig = TRUE, k = 2)

    # TODO
    # Error handling for the cmdscale error
    # Error in cmdscale: 'k' must be in {1, 2, ..  n - 1}

    # one trigger for the cmdscale error is when a user wants to compare
    # one patient vs one patient
    # obviously you cannot do dimensionality reduction on 2 patients

    x <- fit$points[, 1]
    y <- fit$points[, 2]

    df <- data.frame(
      x, y,
      label = row.names(merged_data)
    )
    df$prefix <- substr(df$label, 1, regexpr("_", df$label) - 1)
    # BUG when the from the docker container created files are used
    # the prefix is not correctly extracted

    # in the newest version of phenoRanker
    # the prefix no longer contains the underscore
    # need to mannually add it

    print("renderPlots rv$mappingDf")
    print(rv$mappingDf)

    # print("df")
    # print(str(df))

    df_merged <- merge(
      df,
      rv$mappingDf,
      by.x = "prefix",
      by.y = "id_prefixes",
      all.x = TRUE
    )

    rv$mdsPlot <- ggplot(
      df_merged,
      aes(
        x, y,
        color = .data[["original_fn"]],
        label = label, #no visible binding for global variable 'label'
        customdata = label
      )
      # aes(
      #   x, y,
      #   color = df_merged[["original_fn"]],
      #   label = df_merged[["label"]]
      # )
    ) +
      geom_point() +
      labs(
        title = "Multidimensional Scaling Results",
        x = "Hamming Distance MDS Coordinate 1",
        y = "Hamming Distance MDS Coordinate 2"
      ) +
      theme(
        plot.title = element_text(
          size = 30,
          face = "bold",
          hjust = 0.5
        ),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 15)
      ) +
      scale_color_discrete(
        name = "Cohort"
      )
  } else {
    path <- rv_general$user_dirs$output$cohorts_ranked

    filePath <- paste0(
      path,
      # get_golem_options("cohortModeOutputFolder"),
      # "data/output/rankedCohortMatrixes/",
      "/",
      runId,
      "/",
      runId,
      ".txt"
    )

    if (!file.exists(filePath)) {
      print("file does not exist")
      print("mod_plot_mds_server")
      print("filePath")
      print(filePath)
      return()
    }

    data <- as.matrix(
      readTxt(
        path,
        # get_golem_options("cohortModeOutputFolder"),
        runId = runId,
        row_names = 1
      )
    )

    # ---- multi dimensional scaling results ----
    fit <- cmdscale(data, eig = TRUE, k = 2)

    x <- fit$points[, 1]
    y <- fit$points[, 2]

    df <- data.frame(x, y, label = row.names(data))
    # print("df")
    # print(df)
    aes_func <- aes(
      x, y,
      label = label,
      customdata = label
    )

    if (uploaded_files_count > 1) {
      df$prefix <- substr(df$label, 1, regexpr("_", df$label) - 1)
      print("df2")
      print(df)
      df <- merge(
        df,
        rv$mappingDf,
        by.x = "prefix",
        by.y = "id_prefixes",
        all.x = TRUE
      )
      aes_func <- aes(
        x, y,
        color = .data[["original_fn"]],
        label = .data[["label"]]
      )
    }

    rv$mdsPlot <- ggplot(
      df,
      aes_func,
    ) +
      geom_point() +
      labs(
        title = "Multidimensional Scaling Results",
        x = "Hamming Distance MDS Coordinate 1",
        y = "Hamming Distance MDS Coordinate 2"
      ) +
      theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 15)
      ) +
      scale_color_discrete(
        name = "Cohort"
      )
  }
}

mod_plot_mds_server <- function(
    id,
    rv_general,
    runId = NULL,
    rv = NULL,
    mode = NULL,
    uploaded_files_count = NULL) {
  moduleServer(id, function(input, output, session) {
    if (!is.null(runId)) {
      renderPlots(
        runId,
        rv,
        rv_general,
        mode,
        uploaded_files_count = uploaded_files_count
      )

      output$plot_mds <- renderPlotly({
        plot <- ggplotly(rv$mdsPlot) %>%
          event_register("plotly_selected")
      })
      return(plot)
    }

    output$plot_mds <- renderPlotly({
      validate(
        need(
          !is.null(runId),
          "Click on Rank to generate the plot"
        )
      )
    })

    observeEvent(event_data("plotly_selected"), {
      print("plotly_selected")
      selected_points <- event_data("plotly_selected")
      # get the labels of the selected points
      patientIds <- selected_points$customdata
      print(selected_points)
      print(patientIds)

      output$selected_points <- renderPrint({
        paste("Selected patient IDs: ", paste(patientIds, collapse = ", "))
      })
    })
  })
}
