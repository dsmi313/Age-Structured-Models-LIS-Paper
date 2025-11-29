# Simple smoke test to ensure the Shiny app sources successfully
# and all required packages are available.
app <- shiny::loadApp("length.R")
if (!inherits(app, "shiny.appobj")) {
  stop("Failed to load Shiny app")
}
message("Shiny app loaded successfully")
