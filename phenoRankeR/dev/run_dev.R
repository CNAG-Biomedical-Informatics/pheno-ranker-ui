# Sass code compilation
sass::sass(
  input = sass::sass_file("inst/app/www/custom.sass"), 
  output = "inst/app/www/custom.css", cache = NULL
)

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
# options(shiny.port = httpuv::randomPort())
options(shiny.port = 3839)

# does not yet seem to work?
# options(shiny.autoreload = TRUE)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application

print(getwd())
run_app()
