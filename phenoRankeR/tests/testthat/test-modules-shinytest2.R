library(dotenv)
library(shinytest2)

tmp <- tempfile()
cat("dbDriver=SQlite\n", file = tmp, append = TRUE)
cat("dbDatabase=/home/ivo/projects/bioinfo/cnag/repos/pheno-ranker-ui/shiny.sqlite\n", file = tmp, append = TRUE)

load_dot_env(tmp)

test_that("mod_about_page works", {
  print(Sys.getenv("dbDriver"))
  print(Sys.getenv("dbDatabase"))

  app <- AppDriver$new(
    app = run_app(),
    # app_dir = pkgload::pkg_path(),
    name = "mod_about_page",
    variant = platform_variant(),
    seed = 123,
    shiny_args = list(
      display.mode = "normal"
    )
  )
  app$set_inputs(`mod_about_page_ui` = "click")
  app$expect_values()
  app$stop()
})

# test_that("mod_beacon_api_page works", {
#   app <- AppDriver$new(
#     app_dir = "../../",
#     name = "mod_beacon_api_page",
#     variant = platform_variant(),
#     seed = 123,
#     shiny_args = list(display.mode = "normal")
#   )
#   app$set_inputs(`mod_beacon_api_page_ui` = "click")
#   app$expect_values()
#   app$stop()
# })

# test_that("mod_cohort_mode works", {
#   app <- AppDriver$new(
#     app_dir = "../../",
#     name = "mod_cohort_mode",
#     variant = platform_variant(),
#     seed = 123,
#     shiny_args = list(display.mode = "normal")
#   )
#   app$set_inputs(`mod_cohort_mode_ui` = "click")
#   app$expect_values()
#   app$stop()
# })

# test_that("mod_conv_mode works", {
#   app <- AppDriver$new(
#     app_dir = "../../",
#     name = "mod_conv_mode",
#     variant = platform_variant(),
#     seed = 123,
#     shiny_args = list(display.mode = "normal")
#   )
#   app$set_inputs(`mod_conv_mode_ui` = "click")
#   app$expect_values()
#   app$stop()
# })

# test_that("mod_decision_tree works", {
#   app <- AppDriver$new(
#     app_dir = "../../",
#     name = "mod_decision_tree",
#     variant = platform_variant(),
#     seed = 123,
#     shiny_args = list(display.mode = "normal")
#   )
#   app$set_inputs(`mod_decision_tree_ui` = "click")
#   app$expect_values()
#   app$stop()
# })
