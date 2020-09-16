
#' See the effect of applying a pre-processing function.
#'
#' @param data The data passed to the pre-preprocessing function.
#' @param fun The pre-processing function that operates on data.
#' @param x The X coordinate.
#' @param y The Y coordinate.
#' @param args Arguments passed as a list to the pre-processing function.
#'
#' @return Nothing. Makes a plot.
#' @export
atl_before_after <- function(data,
                             fun, 
                             x = "x", y = "y",
                             args = list()) {
  # check data
  assertthat::assert_that(is.data.frame(data),
                          msg = "before_after: input is not a dataframe")
  
  # function checking will be done automatically
  
  # make data.table
  data.table::setDT(data)
  
  # apply function to data COPY
  data_copy <- data.table::copy(data)
  data_copy <- do.call(fun, c(list(data = data), args))
  
  # check function output
  assertthat::assert_that(is.data.frame(data_copy),
    msg = "before_after: processing result is not a data.frame")
  
  # get title as function arguments
  # start from 5 because 1 is atl_before_after, 2, 3, 4 are data, x, and y
  argument_values <- as.list(match.call()[5:length(match.call())])
  plot_title <- glue::glue('{names(argument_values[1])} = \\
                                          {argument_values[1]}')
  plot_subtitle <- stringr::str_c(glue::glue('{names(argument_values[-1])} = \\
                                          {argument_values[-1]}'), 
                                  collapse = "; ")
  # plot as overlay
  graphics::par(mar = rep(2, 4))
  graphics::plot(data[[x]], data[[y]], type = "o", 
                 col = "steelblue", 
                 xaxt = "n", yaxt = "n", ann = FALSE,
                 lwd = 1, pch = 16, cex = 0.5)
  graphics::lines(data_copy[[x]], data_copy[[y]], type = "l", 
                  col = "red",
                  xaxt = "n", yaxt = "n", ann = FALSE,
                  pch = 16,
                  lwd = 2)
  graphics::title(main = plot_title, font.main = 1,
                  sub = plot_subtitle, font.sub = 3,
                  line = 0.5)
  graphics::legend(x = "bottomleft", 
                   legend = c("Raw data", "Processed data"),
                   col = c("steelblue",
                           "red"),
                   lty = 1,
                   pch = c(16, NA),
                   lwd = c(1, 2),
                   bty = "n")
}
