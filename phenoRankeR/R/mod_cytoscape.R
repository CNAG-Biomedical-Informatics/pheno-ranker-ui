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
#' @importFrom dplyr group_by mutate row_number

mod_cytoscape_mode_layout <- c(
  "            100px             1fr     ",
  "30px        layoutSelector    cyjShiny",
  "150px        btns             cyjShiny",
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

    # threshold for edge creation
    threshold <- 0.5

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

    print("node_colors 1")
    print(node_colors)

    # node_colors <- node_colors[
    #   as.integer(
    #     cut(normalized_node_thresholds,
    #       breaks = length(node_colors),
    #       include.lowest = TRUE
    #     )
    #   )
    # ]

    bins <- cut(
      normalized_node_thresholds,
      breaks = length(node_colors),
      include.lowest = TRUE
    )

    print("bins")
    print(bins)

    print("bin_indices")
    print(as.integer(bins))


    # bins <- cut(normalized_thresholds, breaks=unique_values, include.lowest=TRUE)

    # print("node_colors")
    # print(node_colors)
    

    bin_indices <- as.integer(bins)
    node_colors <- node_colors[bin_indices]
    
    # the target node should be colored black
    # Conditionally color the last node black
    # if (colorLastNodeBlack) {
    #   node_colors[length(node_colors)] <- "black" # Last node in black
    # }
    
    print("node_colors 2")
    print(node_colors)

    df <- data.frame(
      normalized_threshold = normalized_node_thresholds,
      bin_index = bin_indices,
      node_color = node_colors
    )

    print("df")
    print(df)

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
      index <- which(rownames(df) == x)

      # Determine the color: if the node ID matches "T1_Beacon_1", set it to pink; otherwise, use its assigned color
      target_id <- "T1_Beacon_1" # TODO this should no be hardcoded
      node_color <- if(x == target_id) "pink" else df$node_color[index]

      list(data = list(id = x, color = node_color))
    })

    print("node_list")
    print(node_list)

    # TODO
    # only include the edges that have a target node of "T1_Beacon_1"


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

    # filter the edge list to only include edges that have a target node of "T1_Beacon_1"
    target_id <- "T1_Beacon_1"
    edge_list <- edge_list[sapply(edge_list, function(x) x$data$target == target_id)]

    print("edge_list")
    print(edge_list)

    # remove the nodes that are not connected to the target node
    node_list <- node_list[sapply(node_list, function(x) x$data$id %in% c(target_id, sapply(edge_list, function(x) x$data$source)))]

    # Manually create JSON string
    json_nodes <- paste(lapply(node_list, function(x) {
      sprintf('{"data": {"id": "%s", "color": "%s"}}', x$data$id, x$data$color)
    }), collapse = ", ")

    # edgewidth is missing. Here it should be the value of the Jaccard similarity matrix

    json_edges <- paste(lapply(edge_list, function(x) {
      
      # get width of edge in pixels
      edge_width <- paste0(1 + 10 * x$data$weight, "px")

      sprintf(
        '{"data": {"source": "%s", "target": "%s", "weight": "%s", "color": "%s"}}',
        x$data$source,
        x$data$target,
        edge_width,
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
