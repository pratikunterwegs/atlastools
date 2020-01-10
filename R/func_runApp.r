#' runApp
#'
#' @return Opens a shiny app that allows tweaking of patch construction parameters.
#' @export
#'
wat_run_patch_vis_app <- function()
{
  appDir <- glue::glue('{find.package("watlasUtils")}/patch_vis_app/')
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `watlasUtils`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
