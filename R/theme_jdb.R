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
theme_jdb <- function(gridlines = FALSE, text = "sans") {
  ret <- ggplot2::theme_bw()

  if (!gridlines) {
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                panel.grid.minor = ggplot2::element_blank())
  }

  if (text == "fancy") {
    ret <- ret + ggplot2::theme(
      text = element_text(family = "TeX Gyre Pagella")
    )
  }

  return(ret)
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

# below taken with some modifications from Koundinya Desiraju
# https://rpubs.com/Koundy/71792

#' ggplot2 publication theme
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @return Theme elements to make a "publication style"
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'   geom_point() +
#'   theme_pub()
#'
#' @export
theme_pub <- function(base_size = 14, base_family = "helvetica") {
  ret <- ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2), hjust = 0.5),
      text = ggplot2::element_text(),
      panel.background = ggplot2::element_rect(colour = NA),
      plot.background = ggplot2::element_rect(colour = NA),
      panel.border = ggplot2::element_rect(colour = NA),
      axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
      axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      axis.text = ggplot2::element_text(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(),
      panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = grid::unit(0.2, "cm"),
      legend.spacing = grid::unit(0, "cm"),
      legend.title = ggplot2::element_text(face = "italic"),
      plot.margin = grid::unit(c(10, 5, 5, 5), "mm"),
      strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = ggplot2::element_text(face = "bold")
    )
  return(ret)
}

#' ggplot2 wong color fill
#'
#' @param ... parameters to be passed to `ggplot2::discrete_scale()`
#'
#' @return Wong color scale (fill)
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, fill = Species)) +
#'   geom_point(pch = 21) +
#'   theme_pub() +
#'   scale_fill_pub()
#'
#' @export
scale_fill_pub <- function(...) {
      ggplot2::discrete_scale(
        "fill",
        "Publication",
        scales::manual_pal(
          values = c("#0072b2", "#e69f00", "#009e73", "#cc79a7", "#56b4e9", "#d55e00", "#f0e442")
        ),
        ...
      )
}

#' ggplot2 wong color
#'
#' @param ... parameters to be passed to `ggplot2::discrete_scale()`
#'
#' @return Wong color scale
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'   geom_point() +
#'   theme_pub() +
#'   scale_color_pub()
#'
#' @export
scale_color_pub <- function(...) {
      ggplot2::discrete_scale(
        "colour",
        "Publication",
        scales::manual_pal(
          values = c("#0072b2", "#e69f00", "#009e73", "#cc79a7", "#56b4e9", "#d55e00", "#f0e442")
        ),
        ...
     )
}
