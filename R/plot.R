
# Thu Nov 14 13:04:18 2019 ------------------------------
# Y-Axis Filter combinations
# X-Axis Filter vars

#' Plots validity metrics
#'
#' @param x an filtR object
#' @param metric a metric
#' @param caption a caption for each plot
#' @param main a title for each plot
#' @param ... Arguments to be passed to methods
#'
#' @return a plot of effect size and power for all combinations
#'
#' @export

plot.filtr <- function(x, metric, caption, main, ...) {

  if (!inherits(x, "filtR")) {
    stop("use only with \"filtR\" objects")
  }

  x <- tidyr::gather(x, statistic, value, p.value:effect, factor_key = TRUE)

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(
      x[which(x$statistic == metric), ],
      ggplot2::aes(
        x = 100,
        y = eval(rlang::sym("ID")),
        fill = eval(rlang::sym("value"))
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::theme(
        legend.position = "top",
        axis.ticks = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        legend.key.size = grid::unit(0.01, "npc"),
        legend.key.width = grid::unit(0.1, "npc")
      ) +
      ggplot2::scale_fill_gradient2("",
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = (min(x$value[which(x$statistic == metric)], na.rm = T) + max(x$value[which(x$statistic == metric)], na.rm = T)) / 2,
        limits = c(min(x$value[which(x$statistic == metric)], na.rm = T), max(x$value[which(x$statistic == metric)], na.rm = T))
      )
  }

  print(p)
}
