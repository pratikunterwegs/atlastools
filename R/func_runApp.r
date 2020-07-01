#' Run Shiny app to run examples of the patch functions.
#'
#' @return Opens a shiny app that allows tweaking of patch construction parameters.
#' @export
#'
wat_run_patch_vis_app <- function()
{
  appDir <- glue::glue('{find.package("watlastools")}/patch_vis_app/')
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `watlastools`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
