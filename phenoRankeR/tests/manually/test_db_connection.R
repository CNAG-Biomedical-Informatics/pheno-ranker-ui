library(odbc)
odbc_ini_path <- normalizePath("config/odbc")
Sys.setenv(ODBCSYSINI = odbc_ini_path)

print ("ODBCSYSINI:")
print (Sys.getenv("ODBCSYSINI"))

print(odbcListDrivers())

# Define the connection details
# Replace the placeholders with your actual database connection information
driver <- "PostgreSQL" 
server <- "db" 
port <- 5432         
database <- "shiny"
user <- "shiny"
password <- "shiny"

con_string <- paste0(
    "Driver=", driver,
    ";Server=", server,
    ";Port=", port,
    ";Database=", database,
    ";Uid=", user,
    ";Pwd=", password
)

con <- dbConnect(odbc::odbc(), .connection_string = con_string)

if (dbIsValid(con)) {
  cat("Connected to the database successfully!\n")
  # You can now perform database operations using 'con' object
  # For example, you can run queries using dbGetQuery() or execute other database commands.
  # Remember to close the connection when you're done:
  # dbDisconnect(con)
} else {
  cat("Failed to connect to the database. Please check your connection details.\n")
}
dbDisconnect(con)