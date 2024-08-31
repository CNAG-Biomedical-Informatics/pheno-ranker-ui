#' Run the Shiny Application
#'
#' @param golem_opts A list of options to be passed to the `golem` framework.
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
    packageVersion = "0.0.0.9018",

    userDataDir = "../data/user_data/",

    subDirs = list(
      uploads = list(
        pats_mode_refs = "rankInput/patientMode/references",
        pats_mode_targets = "rankInput/patientMode/targets",
        cohort_mode_cohorts = "rankInput/cohortMode/cohorts",
        weights = "weights",
        ontologies = "ontologies",
        config = "config"
      ),
      output = list(
        sim = "simulatedData",
        examples = "inputExamples",
        conv = "convertedData",
        pats_ranked = "rankedPatients",
        cohorts_ranked = "rankedCohorts"
      )
    ),

    rankInputFolder = "../data/uploads/rankInput/",
    patientRankInputRefsFolder = "../data/uploads/rankInput/patientMode/references",

    cohortRankInputFolder = "../data/uploads/rankInput/cohortMode/cohorts",

    weightsUploadFolder = "../data/uploads/weights",
    extraConfigsUploadFolder = "../data/uploads/config",
    ontologyUploadFolder = "../data/uploads/ontologies/",

    inputExamplesInputFolder = "../data/input_examples/phenopacket_store/0.1.18/",
    inputExamplesOutputFolder = "../data/output/inputExamples/",
    simulationOutputFolder = "../data/output/simulatedData/",
    conversionOutputFolder = "../data/output/convertedData/",
    patientModeOutputFolder = "../data/output/rankedPatients/",
    cohortModeOutputFolder = "../data/output/rankedCohortMatrixes/",

    maxIndividuals = 2500,
    jsonViewerPreviewLimit = 10000,

    tempFolder = "../data/temp/", # no longer needed?

    runWithDocker = FALSE,
    PHENO_SIM_BIN = "bff-pxf-simulator",
    PHENO_RANK_BIN = "pheno-ranker",
    PHENO_CSV_CONV_BIN = "csv2pheno-ranker",
    ODBCSYSINI = "../config/odbc",

    dbDriver = Sys.getenv("DB_DRIVER"),
    dbServer = Sys.getenv("DB_IP"),
    dbPort = 5432,
    dbDatabase = Sys.getenv("DB_NAME"),
    dbUser = Sys.getenv("DB_USER"),
    dbPassword = Sys.getenv("DB_PW"),

    shinyProxyDeployed = TRUE,
    keycloakSecured = TRUE,
    playgroundDummyEmail = "pheno.ranker@playground.com"
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
  )
}
