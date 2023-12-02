#' History Sidebar Module
#'#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom gridlayout grid_container grid_card
#' @importFrom shiny NS actionButton
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom shinyjs showElement hideElement click
#' @importFrom lubridate ymd_hms ymd as_datetime days weeks month year
#' @noRd

# TODO
# the changeIcon to checkmark when editing the label
# is not working yet
# JS function is called changeIcon

# TODO
# The history sidebar css is not working yet

mod_show_history_button_ui <- function(id) {
  ns <- NS(id)
  actionButton(
    ns("btn_show_history"), 
    icon("history")
  )
}

mod_history_sidebar_ui <- function(id){
  ns <- NS(id)

  id_mode_mapping <- list(
    ConvertHistorySidebar = "conv_mode",
    SimulateHistorySidebar = "sim_mode",
    PatientHistorySidebar = "patient_mode",
    CohortHistorySidebar = "cohort_mode"
  )
  mode <- id_mode_mapping[id]
  content_id = paste0(c(mode,id,"content"),collapse = "-")

  grid_container(
    id = id,
    layout = c(
      "      250px",
      "  1fr header"
    ),
    gap_size = "0px",
    grid_card(
      area = "header",
      card_header(
        icon("history"),
        "History",
        actionButton(
          ns("btnClose"),
          icon("close"),
          style = "float:right;"
        ),
        class = "history-sidebar-header"
      ),
      card_body(
        tags$head(
          tags$script(
            src = "www/script.js"
          )
        ),
        uiOutput(content_id)
      )   
    )
  )
}

categorize_runs <- function(db_conn, user_id, mode) {
  print("categorize_runs")
  print(db_conn)
  print(user_id)
  print(mode)

  query <- sprintf(
    "SELECT label,run_id,submitted_at FROM jobs WHERE user_id = %d AND mode = '%s' AND status = 'success' ORDER BY submitted_at DESC",
    user_id, mode
  )

  print("query")
  print(query)

  # categorize each run based on the submit time
  # Similar to ChatGPT (Today, yesterday, previous 7 days, previous 30 days, June, May, April, ...)
  past_runs <- dbGetQuery(db_conn, query)
  timestamps <- ymd_hms(past_runs$submitted_at)

  #below should be a function
  timestamp_mapping <- setNames(
    lapply(1:length(timestamps), function(i) {
      list(
        "run_id" = past_runs$run_id[i],
        "label" = past_runs$label[i]
      )
    }),
    timestamps
  )

  current_date <- Sys.Date()

  # Calculate bucket boundaries
  yesterday <- current_date - days(1)
  two_days_ago <- current_date - days(2)
  eight_days_ago <- current_date - weeks(1)
  one_month_ago <- current_date - months(1)

  # categorize each run based on the submit time w/o overlaps
  # masks below return TRUE/FALSE if the timestamp falls into the bucket
  today_mask <- timestamps >= as_datetime(current_date) & timestamps < as_datetime(current_date + days(1))
  yesterday_mask <- timestamps >= as_datetime(yesterday) & timestamps < as_datetime(current_date)
  previous_7days_mask <- timestamps >= as_datetime(eight_days_ago) & timestamps < as_datetime(yesterday)
  previous_30days_mask <- timestamps >= as_datetime(one_month_ago) & timestamps < as_datetime(eight_days_ago)

  month_buckets <- list()
  current_month <- month(current_date)
  current_year <- year(current_date)

  # Loop through each month from the current one back to January
  for (month_diff in 1:(current_month - 1)) {
    first_day_of_month <- ymd(paste(
      current_year, 
      current_month - month_diff, 
      "01"
    ))
    first_day_of_next_month <- ymd(paste(
        current_year, 
        ifelse(
          current_month - month_diff + 1 > 12, 1,
          current_month - month_diff + 1
        ), 
        "01"
      )
    )
    month_x_bucket <- timestamp_mapping[
      as.Date(timestamps) >= first_day_of_month & as.Date(timestamps) < first_day_of_next_month & !(timestamps %in% timestamps[previous_30days_mask])
    ]

    # If there are any timestamps for the current target month, add them to the list
    if (length(month_x_bucket) > 0) {
      # Get the name of the current target month
      month_name <- format(first_day_of_month, "%B")

      # Add the timestamps for the current target month to the list
      month_buckets[[month_name]] <- month_x_bucket
    }
  }

  run_ids_vector_with_labels <- c()
  # TODO better rename "run_ids_vector_with_labels"
  # maybe better run_ids_vector_with_bucket_names
  add_to_vector_if_not_empty <- function(bucket_name, bucket){
    if (length(bucket) > 0) {

      # <<- operator is used to make sure that the vector is updated globally
      run_ids_vector_with_labels <<- c(run_ids_vector_with_labels, bucket_name)
      run_ids_vector_with_labels <<- c(run_ids_vector_with_labels, bucket)
    } 
  }

  add_to_vector_if_not_empty("Today:", timestamp_mapping[today_mask])
  add_to_vector_if_not_empty("Yesterday:", timestamp_mapping[yesterday_mask])
  add_to_vector_if_not_empty("Previous 7 days:", timestamp_mapping[previous_7days_mask])
  add_to_vector_if_not_empty("Previous 30 days:", timestamp_mapping[previous_30days_mask])

  for (month_name in names(month_buckets)) {
    # Get the timestamps for the current month bucket
    month_bucket <- month_buckets[[month_name]]

    add_to_vector_if_not_empty(month_name, month_bucket)
  }

  print("before return")
  return(run_ids_vector_with_labels)
}

return_link_or_label <- function(
  run_ids_vector_with_labels,
  mode, 
  sidebar, 
  session) {
  
  links <- lapply(run_ids_vector_with_labels, function(link_text) {
    if (length(link_text)==1) {

      # If it's a label, create a simple text element
      link <- div(
        h6(link_text),
        style = "margin-top: 15px;"
      )

    } else {
      query <- parseQueryString(session$clientData$url_search)
      link_id <- paste0("link-", link_text$run_id)
      textbox_id <- paste0("textbox-", link_text$run_id)

      linkStyle <- "margin-right: 10px; text-decoration: none; color: black;"
      if (query$mode == mode && query$id == link_text$run_id) {
        linkStyle <- paste0("margin-right: 10px; text-decoration: none; color: blue;")
      }

      link <- div (
        div(
          style = "display: flex; align-items: center;",
          a(
            id = link_id,
            link_text$label,
            href = paste0(
              session$clientData$url_protocol,
              "//",
              session$clientData$url_hostname,
              ":",
              session$clientData$url_port,
              session$clientData$url_pathname,
              "?mode=",
              mode,
              "&id=",
              link_text$run_id
            ),
            onclick = sprintf(
              "event.preventDefault();Shiny.setInputValue('%s', {runId:%s,mode:'%s'});",
              paste0(sidebar,"-old_run_id_clicked"),
              link_text$run_id,
              mode
            ),
            style = linkStyle
          ),
          tags$input(
            id = textbox_id, 
            type = "text", 
            value = link_text$label, 
            style = "display: none; width: 80%; float-left; margin-right: 10px;"
          ),
          a(
            id = sprintf("edit-%s", link_text$run_id),
            icon("edit"),
            href = "#",
            onclick = sprintf(
              "document.getElementById('%s').style.display = 'none'; document.getElementById('%s').style.display = 'block'; changeIcon('%s','%s','%s','%s');",
              link_id,
              textbox_id,
              sprintf("edit-%s", link_text$run_id),
              textbox_id,
              link_id,
              sidebar
            ),
            style = "margin-right: 10px;"
          ),
          a(
            icon("trash"),
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %s, {priority: 'event'});",
              paste0(sidebar,"-old_run_id_trash_clicked"),
              paste0("{'runId': ", link_text$run_id, ", 'label': '", link_text$label, "'}")
            )
          )
        ),
        br()
      )
    }
    # Return the link or label
    tagList(link)
  })
  return(links)
}

mod_show_history_button_server <- function(
  id,
  mode,
  sidebar,
  db_conn) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$btn_show_history, {
      showElement(
        animType = "slide",
        time = 0.5,
        selector = paste0("#",sidebar)
      )

      # TODO
      # below should be not hardcoded
      userId <- 1

      run_ids_vector_with_labels <- categorize_runs(db_conn,userId, mode)
      links <- return_link_or_label(
        run_ids_vector_with_labels, 
        mode, 
        sidebar,
        session
      )

      output$content <- renderUI({
        tagList(links)
      })
    })
  })
}

mod_history_sidebar_server <- function(id, db_conn){
  moduleServer(id,function(input, output, session){
    ns <- session$ns

    observeEvent(input$btnClose, {
      print("close history")
      hideElement(
        selector = paste0("#",id),
        animType = "slide",
        time = 0.5
      )
    })

    observeEvent(input$check_icon_clicked, {  
 
      clicked_icon_id <- input$check_icon_clicked$id
      textbox_value <- input$check_icon_clicked$value

      namespace <- strsplit(ns("check_icon_clicked"), "-")[[1]][1]

      namespace_to_mode_mapping <- list(
        ConvertHistorySidebar = "conv",
        SimulateHistorySidebar = "sim",
        PatientHistorySidebar = "patient",
        CohortHistorySidebar = "cohort"
      )
      
      mode <- namespace_to_mode_mapping[namespace]

      user_id <- 1
      query <- sprintf(
        "UPDATE jobs SET label = '%s' WHERE run_id = '%s' and mode = '%s' and user_id = %d",
        textbox_value, 
        str_remove(clicked_icon_id, "edit-"),
        mode,
        user_id
      )
      dbExecute(db_conn, query)

      outer_namespace <- paste0(mode,"_mode")
      btn_id <- paste(
        outer_namespace,
        namespace,
        "btn_show_history",
        sep = "-"
      )

      # btn_id <- "sim_mode-SimulateHistorySidebar-btn_show_history"
      click(btn_id, asis = TRUE)
    })

    observeEvent(input$old_run_id_clicked, {

      runIdObject <- input$old_run_id_clicked
      run_id <- runIdObject$runId
      mode <- runIdObject$mode

      clicked_link_id <- as.character(run_id)
      print(paste("Link clicked with ID:", clicked_link_id))

      # URL change should trigger get PastRunResults in app_server.R
      session$sendCustomMessage(
        type = "changeURL", 
        message = list(mode=mode,id=clicked_link_id)
      )

      namespace <- strsplit(ns("old_run_id_clicked"), "-")[[1]][1]

      outer_namespace <- paste0(mode,"_mode")
      btn_id <- paste(
        outer_namespace,
        namespace,
        "btn_show_history",
        sep = "-"
      )

      # btn_id <- "sim_mode-SimulateHistorySidebar-btn_show_history"
      click(btn_id, asis = TRUE)
    })

    observeEvent(input$btn_delete_run, {

      namespace <- strsplit(ns("btn_delete_run"), "-")[[1]][1]

      namespace_to_mode_mapping <- list(
        ConvertHistorySidebar = "conv",
        SimulateHistorySidebar = "sim",
        PatientHistorySidebar = "patient",
        CohortHistorySidebar = "cohort"
      )
      
      mode <- namespace_to_mode_mapping[namespace]

      user_id <- 1
      query <- sprintf(
        "DELETE FROM jobs WHERE run_id = '%s' and mode = '%s' and user_id = %d",
        input$old_run_id_trash_clicked$runId,
        mode,
        user_id
      )
      dbExecute(db_conn, query)
      removeModal()

      outer_namespace <- paste0(mode,"_mode")
      btn_id <- paste(
        outer_namespace,
        namespace,
        "btn_show_history",
        sep = "-"
      )

      # btn_id <- "sim_mode-SimulateHistorySidebar-btn_show_history"
      click(btn_id, asis = TRUE)
    })

    observeEvent(input$old_run_id_trash_clicked, {
      runId <- input$old_run_id_trash_clicked$runId
      label <- input$old_run_id_trash_clicked$label

      showModal(modalDialog(
        title = paste("Delete run",runId,"?"),
        paste("This will delete the run with the label:",label),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_delete_run"), "Delete run")
        ),
        easyClose = TRUE,
      ))
    })
  })
}