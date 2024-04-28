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
#' @importFrom cyjShiny cyjShiny cyjShinyOutput renderCyjShiny doLayout
#' @importFrom shinyWidgets colorPickr
#' @importFrom scales brewer_pal viridis_pal
#' @importFrom esquisse palettePicker
#' @importFrom shiny.blueprint reactOutput renderReact MultiSlider MultiSliderHandle
#' @importFrom shiny.react setInput

# TODO
# add shinyjs to add custom event handlers
# e.g. clicking on a node should trigger an update in the table

mod_cytoscape_mode_layout <- c(
  "             200px                       1fr     ",
  "70px        layoutSelector              cyjShiny",
  "80px        thresholdSlider             cyjShiny",
  "70px        targetNodeColorPicker       cyjShiny",
  "70px        referenceNodesColorPicker   cyjShiny",
  "70px        edgesColorPicker            cyjShiny",
  "80px        multiSlider                 cyjShiny",
  "80px        edgesWidthSlider            cyjShiny"
  # "70px        download                    cyjShiny"
)

color_palette_choices <- list(
  "Default" = list(
    "default" = c("red", "green", "blue")
  ),
  "Viridis" = list(
    "viridis" = viridis_pal(option = "viridis")(3),
    "inferno" = viridis_pal(option = "inferno")(3),
    "plasma" = viridis_pal(option = "plasma")(3),
    "cividis" = viridis_pal(option = "cividis")(3)
  ),
  "Brewer" = list(
    "Blues" = brewer_pal(palette = "Blues")(3),
    "Reds" = brewer_pal(palette = "Reds")(3),
    "Paired" = brewer_pal(palette = "Paired")(3),
    "Set1" = brewer_pal(palette = "Set1")(3)
  )
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
        "Minimum Jaccard index",
        min = 0,
        max = 1,
        value = 0.5,
        step = 0.01
      )
    ),
    grid_place(
      area = "targetNodeColorPicker",
      colorPickr(
        ns("targetNodeColorPicker"),
        "Target Node Color",
        selected = "teal"
      )
    ),
    grid_place(
      area = "referenceNodesColorPicker",
      colorPickr(
        ns("referenceNodesColorPicker"),
        "Reference Nodes Color",
        selected = "red"
      )
    ),
    grid_place(
      area = "edgesColorPicker",
      fluidRow(
        palettePicker(
          inputId = ns("edgesColorPicker"),
          label = "Edge color palette",
          choices = color_palette_choices,
          textColor = c(
            rep("white", 5), rep("black", 4)
          )
        )
      )
    ),
    grid_place(
      area = "multiSlider",
      fluidRow(
        p("Edge color thresholds"),
        reactOutput(ns("multiSlider"))
      )
    ),
    grid_place(
      area = "edgesWidthSlider",
      sliderInput(
        ns("edgesWidthSlider"),
        "Edge Width",
        min = 1,
        max = 10,
        value = 5,
        step = 1
      )
    ),
    # grid_place(
    #   area = "download",
    #   actionButton(
    #     ns("downloadGraph"),
    #     "Download Graph"
    #   )
    # ),
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
# Threshold high/mid should not be hardcoded
getColorBasedOnThreshold <- function(
    value,
    thresholdHigh,
    thresholdMid,
    color_palette_name
    ) {

  for (palette_type in names(color_palette_choices)) {
    if (color_palette_name %in% names(color_palette_choices[[palette_type]])) {
      edge_color_palette <- color_palette_choices[[palette_type]][color_palette_name]
      break
    }
  }

  colors_vector <- sub(
    "FF$", "", # remove the alpha channel
    edge_color_palette[[1]]
  )

  if (value >= thresholdHigh) {
    return(colors_vector[3])
  } else if (value >= thresholdMid) {
    return(colors_vector[2])
  } else {
    return(colors_vector[1])
  }
}

# Function to create the cytoscape graph
create_cyto_graph <- function(
  runId,
  jaccard_idx_threshold = 0.5,
  target_node_color = "teal",
  reference_nodes_color = "red",
  edge_color_palette = "default",
  edge_thresholds = c(0.5, 0.9),
  edge_width_multiplier = 5
  ) {

  if (is.null(runId)) {
    return()
  }

  filePath <- paste0(
    get_golem_options("patientModeOutputFolder"),
    runId,
    "/",
    runId,
    "_jaccard.txt"
  )

  if (!file.exists(filePath)) {
    stop("Jaccard file not found")
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
    normalized_similar_pats_count_per_pat <- rep(
      0.5, length(similar_pats_count_per_pat
    ))
  } else {
    # Normalize the counts to a 0-1 scale
    normalized_similar_pats_count_per_pat <- (
      similar_pats_count_per_pat - min_similar_pat_count) /
      (max_similar_pats_count - min_similar_pat_count)
  }

  # BELOW only makes sense for the cohort mode

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
        x, 0.90, 0.50, edge_color_palette
      )
    }
  )

  edge_colors <- matrix(
    edge_colors,
    nrow = nrow(sim_matrix),
    ncol = ncol(sim_matrix)
  )


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
    node <- sprintf(node_template, target_id, target_node_color)
    return(sprintf('{"elements": {"nodes": [%s], "edges": []}}', node))
  }

  # Create node and edge lists
  nodes <- unique(c(
    rownames(sim_matrix)[edges[, 1]],
    colnames(sim_matrix)[edges[, 2]]
  ))


  node_list <- lapply(nodes, function(x) {
    index <- which(rownames(df) == x)
    # node_color <- if (x == target_id) "pink" else df$node_color[index]
    node_color <- if (x == target_id) target_node_color else reference_nodes_color

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
    edge_width <- paste0(edge_width_multiplier * x$data$weight, "px")
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
  print("graph_json")
  print(graph_json)
  return(graph_json)
}

render_multi_slider <- function(
  ns,
  multiSliderMinVal,
  multiSliderHandlersVal, 
  min_val = 0.5, 
  edge_color_palette = "default"
  ) {

  print("render_multi_slider")
  print(min_val)

  max_val <- 1
  available_range <- max_val - min_val
  print("available_range")
  print(available_range)

  mid_val <- (available_range / 3) + min_val
  print("mid_val")
  print(mid_val)
  high_val <- (available_range / 3) * 2 + min_val
  print("high_val")
  print(high_val)

  low_range_color <- getColorBasedOnThreshold(
    mid_val - 0.1, high_val, mid_val, edge_color_palette
  )
  print("low_range_color")
  print(low_range_color)

  mid_range_color <- getColorBasedOnThreshold(
    mid_val, high_val, mid_val, edge_color_palette
  )
  print("mid_range_color")
  print(mid_range_color)

  high_range_color <- getColorBasedOnThreshold(
    high_val, high_val, mid_val, edge_color_palette
  )

  print("high_range_color")
  print(high_range_color)

  renderReact({
    MultiSlider(
      onChange = setInput(ns("multiSliderHandlersVal")),
      min = multiSliderMinVal(),
      max = 1,
      stepSize = 0.01,
      MultiSliderHandle(
        type = "start",
        value = multiSliderHandlersVal()[1],
        trackStyleBefore = list(background = low_range_color),
        trackStyleAfter = list(background = mid_range_color),
        interactionKind = "push"
      ),
      MultiSliderHandle(
        type = "end",
        value = multiSliderHandlersVal()[2],
        trackStyleAfter = list(background = high_range_color),
        interactionKind = "push"
      )
    )
  })
}

mod_cytoscape_server <- function(
    id,
    runId = NULL,
    rv = NULL,
    mode = NULL) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # assuming that min jaccard index is 0.5

    multiSliderMinValDefault <- 0.5
    multiSliderMinVal <- reactiveVal(multiSliderMinValDefault)

    multiSliderHandlersDefault <- c(0.6666667, 0.8333333)
    multiSliderHandlersVal <- reactiveVal(multiSliderHandlersDefault)

    observe(multiSliderHandlersVal(input$multiSliderHandlersVal)) |> bindEvent(input$multiSliderHandlersVal)

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
      
      min_val <- input$thresholdSlider
      max_val <- 1
      available_range <- max_val - min_val
      print("available_range")
      print(available_range)

      mid_val <- (available_range / 3) + min_val
      print("mid_val")
      print(mid_val)
      high_val <- (available_range / 3) * 2 + min_val
      print("high_val")
      print(high_val)

      multiSliderMinVal(min_val)
      multiSliderHandlersVal(c(mid_val, high_val))

      output$cyjShiny <- renderCyjShiny({
        cyjShiny(
          graph_json,
          layoutName = input$layoutSelector,
          styleFile = basicStyleFile
        )
      })
    })

    observeEvent(input$targetNodeColorPicker, {
      print(input$targetNodeColorPicker)
      graph_json <- create_cyto_graph(runId, input$thresholdSlider, input$targetNodeColorPicker)
      output$cyjShiny <- renderCyjShiny({
        cyjShiny(
          graph_json,
          layoutName = input$layoutSelector,
          styleFile = basicStyleFile
        )
      })
    })

    observeEvent(input$referenceNodesColorPicker, {
      graph_json <- create_cyto_graph(
        runId,
        input$thresholdSlider,
        input$targetNodeColorPicker,
        input$referenceNodesColorPicker
      )
      output$cyjShiny <- renderCyjShiny({
        cyjShiny(
          graph_json,
          layoutName = input$layoutSelector,
          styleFile = basicStyleFile
        )
      })
    })

    observeEvent(input$edgesColorPicker, {
      print("edgesColorPicker")
      print(input$edgesColorPicker)
      graph_json <- create_cyto_graph(
        runId,
        input$thresholdSlider,
        input$targetNodeColorPicker,
        input$referenceNodesColorPicker,
        input$edgesColorPicker
      )
      output$cyjShiny <- renderCyjShiny({
        cyjShiny(
          graph_json,
          layoutName = input$layoutSelector,
          styleFile = basicStyleFile
        )
      })
      output$multiSlider <- render_multi_slider(
        ns, multiSliderMinVal,
        multiSliderHandlersVal,
        edge_color_palette = input$edgesColorPicker
      )
    })

    observeEvent(input$multiSliderInput, {
      print("multiSlider")
      print(input$multiSliderInput)
      graph_json <- create_cyto_graph(
        runId,
        input$thresholdSlider,
        input$targetNodeColorPicker,
        input$referenceNodesColorPicker,
        input$edgesColorPicker,
        input$multiSliderInput,
        input$edgesWidthSlider
      )
      output$cyjShiny <- renderCyjShiny({
        cyjShiny(
          graph_json,
          layoutName = input$layoutSelector,
          styleFile = basicStyleFile
        )
      })
    })

    observeEvent(input$edgesWidthSlider, {
      print("edgesWidthSlider")
      print(input$edgesWidthSlider)
      graph_json <- create_cyto_graph(
        runId,
        input$thresholdSlider,
        input$targetNodeColorPicker,
        input$referenceNodesColorPicker,
        input$edgesColorPicker,
        input$multiSlider,
        input$edgesWidthSlider
      )
      output$cyjShiny <- renderCyjShiny({
        cyjShiny(
          graph_json,
          layoutName = input$layoutSelector,
          styleFile = basicStyleFile
        )
      })
    })

    output$multiSlider <- render_multi_slider(
      ns, multiSliderMinVal, multiSliderHandlerVal
    )

    output$cyjShiny <- renderCyjShiny({
      cyjShiny(
        create_cyto_graph(runId),
        layoutName = "fcose",
        styleFile = basicStyleFile
      )
    })
  })
}
