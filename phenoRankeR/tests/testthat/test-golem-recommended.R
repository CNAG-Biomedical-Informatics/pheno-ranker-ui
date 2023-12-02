#' Set golem options for unit testing purposes
#'
#' @param golem_opts list()
#'
#' @examples
#' \dontrun{
#'   my_golem_options <-
#'   list(
#'     myconfig = "config01"
#'   )
#'   set_testing_golem_options(my_golem_options)
#' }
#' found here: https://stackoverflow.com/questions/75361735/run-testserver-w-golemget-golem-options

set_testing_golem_options <-
  function (golem_opts)
  {
    current_golem_options = getShinyOption("golem_options")
    for (n in names(golem_opts)) {
      current_golem_options[[n]] <- golem_opts[[n]]
    }
    shinyOptions(golem_options = current_golem_options)
  }

set_testing_golem_options(
  list(
    rankInputFolder= "../../../data/uploads/rankInput/",
    patientRankInputRefsFolder="../../../data/uploads/rankInput/patientMode/references",

    cohortRankInputFolder="../../../data/uploads/rankInput/cohortMode/cohorts",
    
    weightsUploadFolder="../../../data/uploads/weights",
    extraConfigsUploadFolder="../../../data/uploads/config",
    ontologyUploadFolder="../../../data/uploads/ontologies/",

    simulationOutputFolder="../../../data/output/simulatedData/",
    conversionOutputFolder="../../../data/output/convertedData/",
    patientModeOutputFolder="../../../data/output/rankedPatients/",
    cohortModeOutputFolder="../../../data/output/rankedCohortMatrixes/",

    tempFolder="../../../data/temp/",

    runWithDocker = FALSE,
    PHENO_SIM_BIN = "bff-pxf-simulator",
    PHENO_RANK_BIN = "pheno-ranker",
    PHENO_CSV_CONV_BIN = "csv2pheno_ranker",
    # hardcoded for testing purposes it works
    # ODBCSYSINI = "/home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/config/odbc",
    # ODBCSYSINI = "../../../config/odbc",
    ODBCSYSINI = Sys.getenv("ODBCSYSINI"),
  
    dbDriver = "PostgreSQL",
    dbServer = Sys.getenv("DB_IP"),
    dbPort = 5432,
    dbDatabase = "shiny",
    dbUser = "shiny",
    dbPassword = "shiny"
  )
)

test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(app_ui)
  for (i in c("request")) {
    expect_true(i %in% names(fmls))
  }
})

test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")) {
    expect_true(i %in% names(fmls))
  }
})

test_that(
  "app_sys works",
  {
    expect_true(
      app_sys("golem-config.yml") != ""
    )
  }
)

test_that(
  "golem-config works",
  {
    config_file <- app_sys("golem-config.yml")
    skip_if(config_file == "")

    expect_true(
      get_golem_config(
        "app_prod",
        config = "production",
        file = config_file
      )
    )
    expect_false(
      get_golem_config(
        "app_prod",
        config = "dev",
        file = config_file
      )
    )
  }
)

# Create a mock session
mock_shiny_session <- shiny:::MockShinySession$new()

# Assign the golem options to the session
mock_shiny_session$options[["golem_options"]] <-
(getShinyOption("golem_options"))

# Configure this test to fit your need.
# testServer() function makes it possible to test code in server functions and modules, without needing to run the full Shiny application
testServer(
  app_server, 
  {

  # Set and test an input
  session$setInputs(x = 2)
  expect_equal(input$x, 2)

  # Example of tests you can do on the server:
  # - Checking reactiveValues
  # expect_equal(r$lg, 'EN')
  # - Checking output
  # expect_equal(output$txt, "Text")
  },
  session = mock_shiny_session
)

# Configure this test to fit your need
test_that(
  "app launches",
  {
    golem::expect_running(sleep = 5)
  }
)
