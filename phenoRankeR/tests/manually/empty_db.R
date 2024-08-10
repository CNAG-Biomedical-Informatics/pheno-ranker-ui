library(DBI)
library(jsonlite)
library(odbc)

odbc_ini_path <- normalizePath("../config/odbc")
Sys.setenv(ODBCSYSINI = odbc_ini_path)

print("ODBCSYSINI:")
print(Sys.getenv("ODBCSYSINI"))

print(odbcListDrivers())

db_driver <- Sys.getenv("DB_DRIVER")

con_string <- paste0(
  "Driver=", db_driver,
  ";Server=", Sys.getenv("DB_IP"),
  ";Port=", Sys.getenv("DB_PORT"),
  ";Database=", Sys.getenv("DB_NAME"),
  ";Uid=", Sys.getenv("DB_USER"),
  ";Pwd=", Sys.getenv("DB_PW")
)
print(con_string)
con <- dbConnect(odbc::odbc(), .connection_string = con_string)

# Function to empty (truncate) all tables in the database
empty_database <- function(con) {
  # Get a list of all tables in the database
  tables <- dbListTables(con)

  cmd_end <- " CASCADE;"
  cmd <- "TRUNCATE TABLE "
  if (db_driver == "SQLite") {
    cmd <- "DELETE FROM "
    cmd_end <- ";"
  }

  # Truncate each table
  for (table in tables) {
    query <- paste0(cmd, table, cmd_end)
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
