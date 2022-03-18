#' ggplot2 theme
#'
#' @param gridlines Should grid lines be included in the plot
#'
#' @return Theme elements to make the style I want.
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'   geom_point() +
#'   theme_jdb()
#'
#' @export
theme_jdb <- function(gridlines = FALSE) {
  if (gridlines) ggplot2::theme_bw()
  else ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                            panel.grid.minor = ggplot2::element_blank())
}

#' remove ticks and numbers from axes
#'
#'
#' @return Theme element which removes axis elements
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'   geom_point() +
#'   theme_jdb() +
#'   remove_axis_ticks()
#'
#' @export
remove_axis_ticks <- function() {
  ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                 axis.text  = ggplot2::element_blank())
}
