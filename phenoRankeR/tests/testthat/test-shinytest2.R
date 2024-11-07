library(shinytest2)

test_that("{shinytest2} recording: shinytest2_about_page", {
  app <- AppDriver$new(variant = platform_variant(), name = "shinytest2_about_page", 
      height = 947, width = 1363)
  app$expect_values()
  app$set_inputs(nav = "About")
  app$set_window_size(width = 1363, height = 947)
  app$expect_values()
  app$expect_screenshot()
})
