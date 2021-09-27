runExample <- function() {
  appDir <- system.file("shiny-examples", "apivalm", package = "valmyndigheten_get")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `valmyndigheten_get`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
