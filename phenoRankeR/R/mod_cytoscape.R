#' Table Mode Module
#'
#' @description Renders tables for the patient mode
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS actionButton
#' @importFrom gridlayout grid_container grid_card grid_place
#' @importFrom jsonlite toJSON
#' @importFrom cyjShiny cyjShiny cyjShinyOutput renderCyjShiny dataFramesToJSON doLayout
#' @importFrom qgraph qgraph


mod_cytoscape_mode_layout <- c(
  "            100px             1fr     ",
  "30px        layoutSelector    cyjShiny",
  "150px        btns              cyjShiny",
  "50px        download          cyjShiny"
)

mod_cytoscape_ui <- function(id) {
  ns <- NS(id)

  grid_container(
    layout = mod_cytoscape_mode_layout,
    grid_place(
      area = "layoutSelector",
      selectInput(
        ns("layoutSelector"),
        "Layout",
        choices = c(
          "cola",
          "cose",
          "grid",
          "random",
          "circle"
        ),
        selected = "cola"
      )
    ),
    grid_place(
      area = "btns",
      actionButton(
        ns("getSelectedNodes"),
        "Get Selected Nodes"
      )
    ),
    grid_place(
      area = "download",
      actionButton(
        ns("downloadGraph"),
        "Download Graph"
      )
    ),
    grid_place(
      area = "cyjShiny",
      card_body(
        cyjShinyOutput(
          ns("cyjShiny")
        )
      )
    )
  )
}

# Function to determine color based on threshold values
getColorBasedOnThreshold <- function(
    value,
    thresholdHigh,
    thresholdMid,
    colorHigh,
    colorMid,
    colorLow) {
  if (value > thresholdHigh) {
    return(colorHigh)
  } else if (value > thresholdMid) {
    return(colorMid)
  } else {
    return(colorLow)
  }
}

mod_cytoscape_server <- function(
    id,
    runId = NULL,
    rv = NULL,
    mode = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$layoutSelector, {
      print(input$layoutSelector)
      doLayout(session, input$layoutSelector)
    })

    # TODO
    # This should be the Jaccard similarity matrix

    filePath <- paste0(
      get_golem_options("patientModeOutputFolder"),
      runId,
      "/",
      runId,
      "_jaccard.txt"
    )

    if (!file.exists(filePath)) {
      print("file does not exist")
      return()
    }

    sim_matrix <- as.matrix(
      readTxt(
        get_golem_options("patientModeOutputFolder"),
        runId = runId,
        row_names = 1,
        fileName_suffix = "_jaccard.txt"
      )
    )

    print("sim_matrix")
    print(sim_matrix)

    # Toggle for coloring the last node black
    colorLastNodeBlack <- FALSE

    # threshold for edge creation
    threshold <- 0.8

    # Apply this function to each node and edge
    node_thresholds <- apply(
      sim_matrix, 1, function(x) sum(x > 0.9)
    )

    print("node_thresholds")
    print(node_thresholds)

    max_node_threshold <- max(node_thresholds)
    min_node_threshold <- min(node_thresholds)

    print("max_node_threshold")
    print(max_node_threshold)

    print("min_node_threshold")
    print(min_node_threshold)

    normalized_node_thresholds <- (node_thresholds - min_node_threshold) / (max_node_threshold - min_node_threshold)

    print("(node_thresholds - min_node_threshold)")
    print((node_thresholds - min_node_threshold))

    print("(max_node_threshold - min_node_threshold)")
    print((max_node_threshold - min_node_threshold))

    print("normalized_node_thresholds")
    print(normalized_node_thresholds)

    # Color nodes based on normalized threshold
    node_colors <- colorRampPalette(
      c("red", "green", "blue")
    )(length(unique(normalized_node_thresholds)))

    print("node_colors")
    print(node_colors)

    node_colors <- node_colors[
      as.integer(
        cut(normalized_node_thresholds,
          breaks = length(node_colors),
          include.lowest = TRUE
        )
      )
    ]

    print("node_colors")
    print(node_colors)

    # Conditionally color the last node black
    if (colorLastNodeBlack) {
      node_colors[length(node_colors)] <- "black" # Last node in black
    }

    # Edge colors with similar logic
    edge_colors <- apply(
      sim_matrix,
      c(1, 2),
      function(x) {
        getColorBasedOnThreshold(
          x, 0.90, 0.50, "blue", "green", "red"
        )
      }
    )
    edge_colors <- matrix(
      edge_colors,
      nrow = nrow(sim_matrix),
      ncol = ncol(sim_matrix)
    )

    print("node_colors")
    print(node_colors)

    print("edge_colors")
    print(edge_colors)

    # graph <- qgraph(
    #   sim_matrix,
    #   labels = colnames(sim_matrix),
    #   layout = "spring",
    #   label.font = 2,
    #   vsize = 10,
    #   threshold = 0.50,
    #   shape = "circle",
    #   color = node_colors,
    #   edge.color = edge_colors,
    #   edge.width = 1,
    #   cut = 0.5,
    #   plot = FALSE
    # )

    # print(graph)

    # Identify where the matrix values exceed the threshold
    edges <- which(
      sim_matrix > threshold,
      arr.ind = TRUE
    )

    # Create node and edge lists
    nodes <- unique(c(
      rownames(sim_matrix)[edges[, 1]],
      colnames(sim_matrix)[edges[, 2]]
    ))
    node_list <- lapply(
      nodes,
      function(x) list(data = list(id = x))
    )
    edge_list <- apply(edges, 1, function(x) {
      source <- rownames(sim_matrix)[x[1]]
      target <- colnames(sim_matrix)[x[2]]
      weight <- sim_matrix[x[1], x[2]]
      list(data = list(
        source = source,
        target = target,
        weight = weight
      ))
    })

    # Manually create JSON string
    json_nodes <- paste(lapply(node_list, function(x) {
      sprintf(
        '{"data": {"id": "%s"}}',
        x$data$id
      )
    }), collapse = ", ")

    json_edges <- paste(lapply(edge_list, function(x) {
      sprintf(
        '{"data": {"source": "%s", "target": "%s", "weight": %f}}',
        x$data$source,
        x$data$target,
        x$data$weight
      )
    }), collapse = ", ")

    graph_json <- sprintf(
      '{"elements": {"nodes": [%s], "edges": [%s]}}',
      json_nodes,
      json_edges
    )

    # Example DATA ----
    # tbl_nodes <- data.frame(
    #   id = c("A", "B", "C"),
    #   size = c(10, 20, 30),
    #   stringsAsFactors = FALSE
    # )

    # # Must have the interaction column
    # tbl_edges <- data.frame(
    #   source = c("A", "B", "C"),
    #   target = c("B", "C", "A"),
    #   interaction = c("inhibit", "stimulate", "inhibit"),
    #   stringsAsFactors = FALSE
    # )

    # graph_json <- toJSON(
    #   dataFramesToJSON(tbl_edges, tbl_nodes),
    #   auto_unbox = TRUE
    # )

    output$cyjShiny <- renderCyjShiny({
      cyjShiny(graph_json, layoutName = "cola")
    })
  })
}
