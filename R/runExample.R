#' apivlam shiny result
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runExample()
#' }
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "apivalm")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `apivalm`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
