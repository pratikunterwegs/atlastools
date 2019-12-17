#' runApp
#'
#' @return Opens a shiny app that allows tweaking of patch construction parameters.
#' @export
#'
runTweakApp <- function()
{
  appDir <- "inst/patch_vis_app/"
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `watlasUtils`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
