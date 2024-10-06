#' db Module
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom odbc odbc odbcListDrivers

mod_db_ui <- function(id){
  ns <- NS(id)
}

mod_db_server <- function(id){

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    initialized <- reactiveVal(FALSE)
    conn <- reactiveVal(NULL)

    # cfg <- fromJSON(readLines("config/cfg.json"))
    # dbSettings <- cfg$dbSettings
    # dbSettings <- get_golem_options("dbSettings")
    print("get_golem_options")
    print(get_golem_options())

    dbDriver <- get_golem_options("dbDriver")

    if (dbDriver == "PostgreSQL") {
      con_string <- paste0(
        "Driver=", get_golem_options("dbDriver"),
        ";Server=", get_golem_options("dbServer"),
        ";Port=", get_golem_options("dbPort"),
        ";Database=", get_golem_options("dbDatabase"),
        ";Uid=", get_golem_options("dbUser"),
        ";Pwd=", get_golem_options("dbPassword")
      )
    } else if (dbDriver == "SQLite") {
      con_string <- paste0(
        "Driver=", get_golem_options("dbDriver"),
        ";Database=", get_golem_options("dbDatabase")
      )
    } else {
      print("dbDriver not supported")
    }

    print("con_string")
    print(con_string)

    print("cfg")
    # print(cfg)

    # Sys.setenv(ODBCSYSINI = cfg$ODBCSYSINI)
    Sys.setenv(ODBCSYSINI = get_golem_options("ODBCSYSINI"))
    print(Sys.getenv("ODBCSYSINI"))

    print("Available Drivers:")
    print(odbcListDrivers())

    # TODO
    # return the db_conn object
    db_conn <- dbConnect(odbc(), .connection_string = con_string)
    # TODO
    # throw an error if the db_conn is not initialized

    create_user_table <- "
      CREATE TABLE IF NOT EXISTS users (
      id SERIAL PRIMARY KEY,
      email varchar(255) NOT NULL UNIQUE
    )"

    create_beacons_table <- "
      CREATE TABLE IF NOT EXISTS beacons (
      id SERIAL PRIMARY KEY,
      url varchar(255) NOT NULL UNIQUE,
      added_by_user_with_id numeric NOT NULL,
      added_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"

    # mapping table to see which user has access to which beacons
    create_user2beacons_table <- "
      CREATE TABLE IF NOT EXISTS user2beacons (
      user_id numeric NOT NULL,
      beacon_id numeric NOT NULL
    )"

    if (dbDriver == "SQLite") {
      create_user_table <- "
        CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY,
        email varchar(255) NOT NULL UNIQUE
      )"

      create_beacons_table <- "
        CREATE TABLE IF NOT EXISTS beacons (
        id INTEGER PRIMARY KEY,
        url varchar(255) NOT NULL UNIQUE,
        added_by_user_with_id numeric NOT NULL,
        added_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
      )"

      create_user2beacons_table <- "
        CREATE TABLE IF NOT EXISTS user2beacons (
        user_id numeric NOT NULL,
        beacon_id numeric NOT NULL
      )"
    }

    # initialize jobs table

    #* NOTE
    # JSONB is only available in sqlite > 3.45.0
    # planned for 2024-01-31

    # TODO
    # user_id probably should be a foreign key

    create_jobs_table <- "
      CREATE TABLE IF NOT EXISTS jobs (
      id SERIAL PRIMARY KEY,
      run_id numeric NOT NULL,
      user_id numeric NOT NULL,
      mode varchar(255) NOT NULL,
      label varchar(255) NOT NULL,
      settings JSONB NOT NULL, 
      status varchar(255) NOT NULL,
      submitted_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"

    if (dbDriver == "SQLite") {
      create_jobs_table <- "
        CREATE TABLE IF NOT EXISTS jobs (
        id INTEGER PRIMARY KEY,
        run_id numeric NOT NULL,
        user_id numeric NOT NULL,
        mode varchar(255) NOT NULL,
        label varchar(255) NOT NULL,
        settings varchar NOT NULL, 
        status varchar(255) NOT NULL,
        submitted_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
      )"
    }

    # TODO
    # create a meta data table per mode to store all the possible settings
    # this way we could retrieve the settings for an old run job and preconfigure the settings
    # based on this job

    # create_patient_mode_settings_table <- 

    observe({
      dbExecute(db_conn, create_user_table)
      dbExecute(db_conn, create_beacons_table)
      dbExecute(db_conn, create_user2beacons_table)
      dbExecute(db_conn, create_jobs_table)

      # update reactive values
      conn(db_conn)
      initialized(TRUE)
    })

    session$onSessionEnded(function() {
      print("session ended")
      dbDisconnect(db_conn)
      print("database connection closed")
    })

    return(list(
      initialized = initialized,
      conn = conn
    ))
  })
}