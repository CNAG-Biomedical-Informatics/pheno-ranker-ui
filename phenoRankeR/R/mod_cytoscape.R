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
          "circle",
          "cola",
          "cose",
          "fcose",
          "grid",
          "random"
        ),
        selected = "fcose"
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
    colorLastNodeBlack <- TRUE

    # threshold for edge creation
    threshold <- 0.4

    # Apply this function to each node and edge
    node_thresholds <- apply(
      sim_matrix, 1, function(x) sum(x > threshold)
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

    # print("edge_colors")
    # print(edge_colors)

    print("colnames(sim_matrix)")
    print(colnames(sim_matrix))

    # Identify where the matrix values exceed the threshold
    edges <- which(
      sim_matrix > threshold,
      arr.ind = TRUE
    )

    # but remove the diagonal
    edges <- edges[edges[, 1] != edges[, 2], ]

    # Create node and edge lists
    nodes <- unique(c(
      rownames(sim_matrix)[edges[, 1]],
      colnames(sim_matrix)[edges[, 2]]
    ))

    node_list <- lapply(nodes, function(x) {
      # TODO
      # better understand how the node coloring is working
      # I do not want to color the last node black
      # but the node with the target id
      # In the beacon example it should be T|Beacon_1
      list(data = list(id = x, color = node_colors[match(x, nodes)]))
    })

    print("node_list")
    print(node_list)

    edge_list <- apply(edges, 1, function(x) {
      source <- rownames(sim_matrix)[x[1]]
      target <- colnames(sim_matrix)[x[2]]
      weight <- sim_matrix[x[1], x[2]]
      list(data = list(
        source = source,
        target = target,
        weight = weight,
        color = edge_colors[x[1], x[2]]
      ))
    })

    # Manually create JSON string
    json_nodes <- paste(lapply(node_list, function(x) {
      sprintf('{"data": {"id": "%s", "color": "%s"}}', x$data$id, x$data$color)
    }), collapse = ", ")

    # edgewidth is missing. Here it should be the value of the Jaccard similarity matrix

    json_edges <- paste(lapply(edge_list, function(x) {
      sprintf(
        '{"data": {"source": "%s", "target": "%s", "weight": %f, "color": "%s"}}',
        x$data$source,
        x$data$target,
        x$data$weight,
        x$data$color
      )
    }), collapse = ", ")

    graph_json <- sprintf(
      '{"elements": {"nodes": [%s], "edges": [%s]}}',
      json_nodes,
      json_edges
    )

    print(graph_json)

    basicStyleFile <- system.file(
      "extdata",
      "cytoscape_styles",
      "basicStyle.js",
      package = "phenoRankeR"
    )

    output$cyjShiny <- renderCyjShiny({
      cyjShiny(
        graph_json,
        layoutName = "fcose",
        styleFile = basicStyleFile
      )
    })
  })
}
