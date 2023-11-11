library(DBI)
library(jsonlite)
library(odbc)

odbc_ini_path <- normalizePath("config/odbc")
Sys.setenv(ODBCSYSINI = odbc_ini_path)

print ("ODBCSYSINI:")
print (Sys.getenv("ODBCSYSINI"))

print(odbcListDrivers())

cfg <- fromJSON(readLines("config/cfg.json"))
dbSettings <- cfg$dbSettings
print(dbSettings)
con_string <- paste0(
    "Driver=", dbSettings$driver,
    ";Server=", dbSettings$server,
    ";Port=", dbSettings$port,
    ";Database=", dbSettings$database,
    ";Uid=", dbSettings$user,
    ";Pwd=", dbSettings$password
)
print(con_string)
con <- dbConnect(odbc::odbc(), .connection_string = con_string)

# Function to empty (truncate) all tables in the database
empty_database <- function(con) {
  # Get a list of all tables in the database
  tables <- dbListTables(con)
  
  # Truncate each table
  for (table in tables) {
    query <- paste0("TRUNCATE TABLE ", table, " CASCADE;")
    print("query:")
    print(query)
    dbExecute(con, query)
    cat("Table", table, "has been truncated.\n")
  }
}

# Call the function to empty the database
empty_database(con)

# Close the database connection
dbDisconnect(con)
