library(dotenv)
library(shinytest2)

tmp <- tempfile()
cat("dbDriver=SQlite\n", file = tmp, append = TRUE)
load_dot_env(tmp)

record_test()
