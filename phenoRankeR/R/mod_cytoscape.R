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
  "             200px             1fr     ",
  "100px        layoutSelector    cyjShiny",
  "100px        thresholdSlider   cyjShiny",
  "150px        btns              cyjShiny",
  "50px         download          cyjShiny"
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
      area = "thresholdSlider",
      sliderInput(
        ns("thresholdSlider"),
        "Jaccard index threshold",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.01
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

# Function to create the cytoscape graph
create_cyto_graph <- function(runId, jaccard_idx_threshold = 0.5) {
  
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

  # remove the diagonal from the matrix
  print("sim_matrix")
  print(sim_matrix)

  # column count where jaccard index <= jaccard_idx_threshold
  # check row-wise
  similar_pats_count_per_pat <- apply(
    sim_matrix,
    1, # apply to rows
    function(x) sum(x >= jaccard_idx_threshold)
  )

  # max/min column count where jaccard index <= jaccard_idx_threshold
  max_similar_pats_count <- max(similar_pats_count_per_pat)
  min_similar_pat_count <- min(similar_pats_count_per_pat)

  # Check if all values are the same
  if (max_similar_pats_count == min_similar_pat_count) {
    # Assign a default value
    normalized_similar_pats_count_per_pat <- rep(0.5, length(similar_pats_count_per_pat))
  } else {
    # Normalize the counts to a 0-1 scale
    normalized_similar_pats_count_per_pat <- (
      similar_pats_count_per_pat - min_similar_pat_count) /
      (max_similar_pats_count - min_similar_pat_count)
  }

  # prepare the color gradient for the nodes
  # TODO: let the user choose the colors
  palette <- c("red", "green", "blue")

  # interpolate the colors based on the number of unique values
  unique_values_count <- length(unique(normalized_similar_pats_count_per_pat))
  node_colors <- colorRampPalette(palette)(unique_values_count)

  print("node_colors")
  print(node_colors)

  if(length(node_colors) > 1) {
    bin_indeces <- as.integer(cut(
      normalized_similar_pats_count_per_pat,
      breaks = length(node_colors),
      include.lowest = TRUE
    ))
  } else {
    bin_indeces <- rep(1, length(normalized_similar_pats_count_per_pat))
    print("else")
  }

  print("bin_indeces")
  print(bin_indeces)

  df <- data.frame(
    similar_pats_count = similar_pats_count_per_pat,
    normalized_similar_pats_count = normalized_similar_pats_count_per_pat,
    bin_index = bin_indeces,
    node_color = node_colors[bin_indeces]
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

  # print("edge_colors")
  # print(edge_colors)

  # Identify where the matrix values exceed the threshold
  edges <- which(
    sim_matrix >= jaccard_idx_threshold,
    arr.ind = TRUE
  )

  # remove the diagonal
  edges <- edges[edges[, 1] != edges[, 2], ]

  target_id <- colnames(sim_matrix)[length(colnames(sim_matrix))]
  if (length(edges) == 0) {
    print("No edges found")
    node_template <- '{"data": {"id": "%s", "color": "%s"}}'
    node <- sprintf(node_template, target_id, "pink")
    return(sprintf('{"elements": {"nodes": [%s], "edges": []}}', node))
  }
  
  # Create node and edge lists
  nodes <- unique(c(
    rownames(sim_matrix)[edges[, 1]],
    colnames(sim_matrix)[edges[, 2]]
  ))


  node_list <- lapply(nodes, function(x) {
    index <- which(rownames(df) == x)
    node_color <- if (x == target_id) "pink" else df$node_color[index]

    list(data = list(id = x, color = node_color))
  })

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

  # filter the edge list to only include edges connected to the target node"
  edge_list <- edge_list[sapply(edge_list, function(x) x$data$target == target_id)]

  print("edge_list")
  print(edge_list)

  # remove the nodes that are not connected to the target node
  node_list <- node_list[
    sapply(
      node_list,
      function(x) x$data$id %in% c(target_id,sapply(edge_list, function(x) x$data$source))
    )
  ]

  # Manually create JSON string
  json_nodes <- paste(lapply(node_list, function(x) {
    sprintf('{"data": {"id": "%s", "color": "%s"}}', x$data$id, x$data$color)
  }), collapse = ", ")

  json_edges <- paste(lapply(edge_list, function(x) {
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
  return(graph_json)
}

mod_cytoscape_server <- function(
    id,
    runId = NULL,
    rv = NULL,
    mode = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    basicStyleFile <- system.file(
      "extdata",
      "cytoscape_styles",
      "basicStyle.js",
      package = "phenoRankeR"
    )

    observeEvent(input$layoutSelector, {
      print(input$layoutSelector)
      doLayout(session, input$layoutSelector)
    })

    observeEvent(input$thresholdSlider, {
      print(input$thresholdSlider)
      graph_json <- create_cyto_graph(runId, input$thresholdSlider)
      output$cyjShiny <- renderCyjShiny({
        cyjShiny(
          graph_json,
          layoutName = input$layoutSelector,
          styleFile = basicStyleFile
        )
      })
    })

    output$cyjShiny <- renderCyjShiny({
      cyjShiny(
        create_cyto_graph(runId),
        layoutName = "fcose",
        styleFile = basicStyleFile
      )
    })
  })
}
