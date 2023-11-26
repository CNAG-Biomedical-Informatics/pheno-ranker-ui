#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_app <- function(
  onStart = NULL,
  options = list(
    # display.mode = 'showcase' <- does not seem to be supported by golem
  ),
  enableBookmarking = NULL,
  uiPattern = "/",
  golem_opts = list(
    rankInputFolder= "../data/uploads/rankInput/",
    patientRankInputRefsFolder="../data/uploads/rankInput/patientMode/references",

    cohortRankInputFolder="../data/uploads/rankInput/cohortMode/cohorts",
    
    weightsUploadFolder="../data/uploads/weights",
    extraConfigsUploadFolder="../data/uploads/config",
    ontologyUploadFolder="../data/uploads/ontologies/",

    simulationOutputFolder="../data/output/simulatedData",
    conversionOutputFolder="../data/output/convertedData/",
    patientModeOutputFolder="../data/output/rankedPatients/",
    cohortModeOutputFolder="../data/output/rankedCohortMatrixes/",

    tempFolder="../data/temp/",

    runWithDocker = FALSE,
    PHENO_SIM_BIN = "bff-pxf-simulator",
    PHENO_RANK_BIN = "pheno-ranker",
    PHENO_CSV_CONV_BIN = "csv2pheno-ranker",
    ODBCSYSINI = "../config/odbc",

    dbDriver = "PostgreSQL",
    dbServer = "10.10.0.2",
    dbPort = 5432,
    dbDatabase = "shiny",
    dbUser = "shiny",
    dbPassword = "shiny"
  ),
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = golem_opts
    # golem_opts = list(
    #   rankInputFolder= "../data/uploads/rankInput/",
    #   patientRankInputRefsFolder="../data/uploads/rankInput/patientMode/references",

    #   cohortRankInputFolder="../data/uploads/rankInput/cohortMode/cohorts",
      
    #   weightsUploadFolder="../data/uploads/weights",
    #   extraConfigsUploadFolder="../data/uploads/config",
    #   ontologyUploadFolder="../data/uploads/ontologies/",

    #   simulationOutputFolder="../data/output/simulatedData/",
    #   conversionOutputFolder="../data/output/convertedData/",
    #   patientModeOutputFolder="../data/output/rankedPatients/",
    #   cohortModeOutputFolder="../data/output/rankedCohortMatrixes/",

    #   tempFolder="../data/temp/",

    #   runWithDocker = FALSE,
    #   PHENO_SIM_BIN = "bff-pxf-simulator",
    #   PHENO_RANK_BIN = "pheno-ranker",
    #   PHENO_CSV_CONV_BIN = "csv2pheno_ranker",
    #   ODBCSYSINI = "../config/odbc",
    #   dbSettings = list(
    #     driver = "PostgreSQL",
    #     server = "10.10.0.2",
    #     port = 5432,
    #     database = "shiny",
    #     user = "shiny",
    #     password = "shiny"
    #   )
    # )
  )
}
