#' Drag and drop Module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom jsonlite fromJSON

 # TODO
# include/exclude are mutually exclusive
# meaning when the include list is populated, the exclude list should be empty
# and vice versa

# use ranked_list update to explain the logic
# + deactivate the other list as drop target
# e.g. include (deactivated because exclude is populated)
# e.g. exclude (deactivated because include is populated)

mod_dnd_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("incl_excl_list"))
}

incl_excl_criteria <- fromJSON(
  readLines(
    "inst/extdata/config/incl_excl_criteria.json"
  )
)

render_dnd <- function(ns,inputFormat=NULL) {

  print("render_dnd")
  labels = c ("")
  if (!is.null(inputFormat)){
    labels = incl_excl_criteria[[inputFormat]]
  }  
  print("labels")
  print(labels)

  print(ns("dnd_target_incl"))
  
  renderUI({
    bucket_list(
      header = "Drag and drop variables to include or exclude them from the comparison",
      group_name = ns("dnd_criteria_group"),
      add_rank_list(
        input_id = ns("dnd_source"),
        text = "",
        labels = labels
      ),
      add_rank_list(
        text = "include",
        labels = NULL,
        input_id = ns("dnd_target_incl")
      ),
      add_rank_list(
        text = "exclude",
        labels = NULL,
        input_id = ns("dnd_target_excl")
      )
    )
  })
}

mod_dnd_server <- function(id, rv_patient){
  moduleServer(id,function(input, output, session){
    ns <- session$ns
    output$incl_excl_list <- render_dnd(ns)

    observeEvent(rv_patient$inputFormat,{
      output$incl_excl_list <- render_dnd(ns,rv_patient$inputFormat)
    })

    # TODO
    # add observeEvent for dnd_target_incl and dnd_target_excl
    # to throw an error when both are populated

    observeEvent(input$dnd_target_incl,{
      print("dnd_target_incl changed")

      # TODO
      # check current state of dnd_target_excl
      # or
      # better deactivate the other list as drop target
    })

    observeEvent(input$dnd_target_excl,{
      print("dnd_target_excl changed")

      # TODO
      # check current state of dnd_target_excl
      # or 
      # better deactivate the other list as drop target
    })
  })
}