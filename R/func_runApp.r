#' runApp
#'
#' @return Opens a shiny app that allows tweaking of patch construction parameters.
#' @export
#'
runTweakApp <- function()
{
  appDir <- system.file("shiny_app", package = "watlasUtils")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `watlasUtils`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", options(shiny.testmode = FALSE))
}
