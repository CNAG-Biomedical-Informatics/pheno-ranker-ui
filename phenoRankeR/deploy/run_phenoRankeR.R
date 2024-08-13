library(phenoRankeR)
options(shiny.port = 3838, shiny.host = "0.0.0.0")

# A function to parse arguments into a named list
parse_args <- function(args) {
  opts <- list()
  num_args <- length(args)
  
  # If there are no arguments, return the empty list
  if (num_args == 0) {
    return(opts)
  }

  if (length(args) %% 2 != 0) {
    stop("An even number of arguments is required, each option should be followed by its value.")
  }
  
  for (i in seq(1, length(args), by = 2)) {
    key <- args[i]
    value <- args[i + 1]
    opts[[key]] <- value
  }
  return(opts)
}

default_golem_opts <- list(
  packageVersion = "0.0.0.9017",

  rankInputFolder = "/app/data/uploads/rankInput/",
  patientRankInputRefsFolder = "/app/data/uploads/rankInput/patientMode/references",

  cohortRankInputFolder = "/app/data/uploads/rankInput/cohortMode/cohorts",

  weightsUploadFolder = "/app/data/uploads/weights",
  extraConfigsUploadFolder = "/app/data/uploads/config",
  ontologyUploadFolder = "/app/data/uploads/ontologies/",

  inputExamplesInputFolder = "/app/data/input_examples/phenopacket_store/0.1.18/",
  inputExamplesOutputFolder = "/app/data/output/inputExamples/",
  simulationOutputFolder = "/app/data/output/simulatedData/",
  conversionOutputFolder = "/app/data/output/convertedData/",
  patientModeOutputFolder = "/app/data/output/rankedPatients/",
  cohortModeOutputFolder = "/app/data/output/rankedCohortMatrixes/",

  maxIndividuals = 2500,

  tempFolder = "/app/data/temp/",  # no longer needed?

  runWithDocker = "True",
  LD_LIB_PATH = "/usr/local/lib/perl5/5.36.1/x86_64-linux-gnu/CORE",
  PHENO_RANK_BIN = "/usr/share/pheno-ranker/bin/pheno-ranker",
  PHENO_SIM_BIN = "/usr/share/pheno-ranker/utils/bff_pxf_simulator/bff-pxf-simulator",
  PHENO_CSV_CONV_BIN = "/usr/share/pheno-ranker/utils/csv2pheno_ranker/csv2pheno-ranker",
  ODBCSYSINI = "/app/config/odbc",
  
  dbDriver = "SQLite",
  dbServer = ":memory:",
  dbPort = 5432,
  dbDatabase = "shiny",
  dbUser = "shiny",
  dbPassword = "shiny"
)

# Get the arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
# Parse the command line arguments into a list
cli_golem_opts <- parse_args(args)

# Update default golem options with any command line arguments provided
updated_golem_opts <- modifyList(default_golem_opts, cli_golem_opts)

run_app(
  golem_opts = updated_golem_opts
)