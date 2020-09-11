
atl_before_after <- function(data,
                             fun, ...) {
  # check data
  
  # check function
  
  # parse function arguments passed to ...
  
  # apply function to data COPY
  data_copy <- data.table::copy(data)
  data_copy <- fun(data_copy, ...)

  # check function output
  assertthat::assert_that(is.data.frame(data_copy),
    msg = "before_after: processing result is not a data.frame")
  
  # set graphics parameters
  # graphics::par(mar = c(2, 2, 2, 2))
  
  # get title as function arguments
  argument_values <- as.list(match.call()[3:length(match.call())])
  plot_title <- glue::glue('{names(argument_values[1])} = \\
                                          {argument_values[1]}')
  plot_subtitle <- stringr::str_c(glue::glue('{names(argument_values[-1])} = \\
                                          {argument_values[-1]}'), 
                                  collapse = "; ")
  # plot as overlay
  graphics::par(mar = rep(2, 4))
  graphics::plot(data$x, data$y, type = "o", 
                 col = "steelblue", 
                 xaxt = "n", yaxt = "n", ann = FALSE,
                 lwd = 1, pch = 16, cex = 0.5)
  graphics::lines(data_copy$x, data_copy$y, type = "l", 
                  col = "red",
                  xaxt = "n", yaxt = "n", ann = FALSE,
                  pch = 16,
                  lwd = 2)
  graphics::title(main = plot_title, font.main = 1,
                  sub = plot_subtitle, font.sub = 3,
                  line = 0.5)
  graphics::legend(x = "bottomleft", 
                   # y = min(data$y) + diff(range(data$y)) / 10,
                   legend = c("Raw data", "Processed data"),
                   col = c("steelblue",
                           "red"),
                   lty = 1,
                   pch = c(16, NA),
                   lwd = c(1, 2),
                   bty = "n")
}
