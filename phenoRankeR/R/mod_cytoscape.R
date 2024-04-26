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
#' @importFrom cyjShiny cyjShiny cyjShinyOutput renderCyjShiny dataFramesToJSON


mod_cytoscape_mode_layout <- c(
  "           500px           1fr     ",
  "30px       layoutSelector  cyjShiny"
  # "600px      btns            cyjShiny",
  # "180px      download        cyjShiny"
)

mod_cytoscape_ui <- function(id) {
  ns <- NS(id)

  # card_body(

  # )
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
      cyjShinyOutput(
        ns("cyjShiny")
      )
    )
  )

  # card_body(
  #   cyjShinyOutput(
  #     ns("cyjShiny")
  #   )
  # )
}

mod_cytoscape_server <- function(
    id,
    runId = NULL,
    rv = NULL,
    mode = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filePath <- paste0(
      get_golem_options("patientModeOutputFolder"),
      runId,
      "/",
      runId,
      ".txt"
    )

    if (!file.exists(filePath)) {
      print("file does not exist")
      return()
    }

    # this needs to be a jaccard similarity matrix
    # sim_matrix <- as.matrix(
    #   read.table(
    #     "matrix.txt",
    #     header = TRUE,
    #     row.names = 1
    #   )
    # )

    sim_matrix <- as.matrix(
      readTxt(
        get_golem_options("patientModeOutputFolder"),
        runId = runId,
        row_names = 1
      )
    )

    print("dim(sim_matrix)")
    print(dim(sim_matrix))

    # threshold for edge creation
    threshold <- 0.8

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
