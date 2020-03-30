#' Title
#'
#' @param x fairness object
#'
#' @return ggplot object
#' @export
#'
#' @import ggplot2
#' @import DALEX
#' @import RColorBrewer
#'
#' @rdname plot_stacked_barplot



plot_stacked_barplot <- function(x) UseMethod("plot_stacked_barplot")

#' @export
#' @rdname plot_stacked_barplot

plot_stacked_barplot.fairness_object <- function(x){

  expanded_data <- expand_fairness_object(x)

  expanded_data <- as.data.frame(expanded_data)
  colnames(expanded_data) <- c("metric","model","score")
  expanded_data$metric <- as.factor(expanded_data$metric)
  expanded_data$model <- as.factor(expanded_data$model)
  expanded_data$score <- round(as.numeric(expanded_data$score),3)

  plot_stacked_barplot.deafult(expanded_data)
}

#' @export
#' @rdname plot_stacked_barplot

plot_stacked_barplot.deafult <- function(x){

  ggplot(x, aes(x = reorder(model, -score), y = score, fill = reorder(metric, score))) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    coord_flip() +
    theme_drwhy_vertical() +
    scale_fill_brewer(palette = "Accent") +
    xlab("Model Label") +
    ylab("Cummulated metric score") +
    labs(fill = "Model Label") +
    ggtitle("Stacked Metric Chart")

}

