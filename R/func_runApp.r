#' runApp
#'
#' @return Opens a shiny app that allows tweaking of patch construction parameters.
#' @export
#'
#' @examples
runTweakApp <- function()
{
  appDir <- system.file("shiny-examples", "tweak_patch_param", package = "watlasUtils")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `watlasUtils`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
