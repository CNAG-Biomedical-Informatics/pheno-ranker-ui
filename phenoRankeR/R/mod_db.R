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

    # cfg <- fromJSON(readLines("config/cfg.json"))
    # dbSettings <- cfg$dbSettings
    # dbSettings <- get_golem_options("dbSettings")
    print("get_golem_options")
    print(get_golem_options())
    # print("dbSettings")
    # print(dbSettings)
    # con_string <- paste0(
    #   "Driver=", dbSettings$driver,
    #   ";Server=", dbSettings$server,
    #   ";Port=", dbSettings$port,
    #   ";Database=", dbSettings$database,
    #   ";Uid=", dbSettings$user,
    #   ";Pwd=", dbSettings$password
    # )

    con_string <- paste0(
      "Driver=", get_golem_options("dbDriver"),
      ";Server=", get_golem_options("dbServer"),
      ";Port=", get_golem_options("dbPort"),
      ";Database=", get_golem_options("dbDatabase"),
      ";Uid=", get_golem_options("dbUser"),
      ";Pwd=", get_golem_options("dbPassword")
    )

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

    # initialize jobs table
    
    # TODO
    # also create a users table if not exists
    observe({
      print("initialize jobs table")
      print(db_conn)
      query <- "
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
      dbExecute(db_conn, query)
      print("HERE")
    })

    # TODO

    # check if user is already in users table
    # if not add user to users table
    # observe({
    #   print("add user to users table")
    #   print(db_conn)
    #   query <- paste0(
    #     "INSERT INTO users (user_id) VALUES (",
    #     session$user,
    #     ")"
    #   )
    #   dbExecute(db_conn, query)
    # })

    session$onSessionEnded(function() {
      print("session ended")
      dbDisconnect(db_conn)
      print("database connection closed")
    })

    return (db_conn)
  })
}