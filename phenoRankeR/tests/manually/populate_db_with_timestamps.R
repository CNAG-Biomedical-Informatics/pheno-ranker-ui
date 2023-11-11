library(DBI)
library(jsonlite)
library(lubridate)
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

user_id <- 123

# Define date range
start_date <- ymd_hms("2023-01-01 00:00:00")
end_date <- ymd_hms("2023-07-26 10:00:00")

# Calculate total minutes in the date range
# total_minutes <- as.numeric(difftime(end_date, start_date, units = "mins"))

total_seconds <- as.numeric(difftime(end_date, start_date, units = "secs"))

## Generate random input data
n_samples <- 100  # Adjust to your needs
timestamps <- as.POSIXct(start_date) + runif(n_samples, 0, total_seconds)
modes <- sample(c("patient"), n_samples, replace = TRUE)

current_date <- force_tz(Sys.time(),"UTC")
yesterday <- current_date - days(1)
two_days_ago <- current_date - days(2)
eight_days_ago <- current_date - weeks(1)
one_month_ago <- current_date - months(1)

# add one timestamp for each boundary
timestamps <- c(current_date, yesterday, two_days_ago, eight_days_ago, one_month_ago, timestamps)
print(timestamps)

sql_template <- "INSERT INTO jobs (run_id, user_id, mode, label, status, submitted_at) VALUES ('%s' ,%d, '%s', '%s','success', '%s')"
for (i in seq_len(n_samples)) {
  sql_query <- sprintf(
    sql_template, 
    format(timestamps[i], "%Y%m%d%H%M%S"),
    user_id, 
    modes[i], 
    paste0("test",i),
    format(timestamps[i], "%Y-%m-%d %H:%M:%S")
  )
  dbExecute(con, sql_query)
}

dbDisconnect(con)

