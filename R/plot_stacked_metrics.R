#' Plot stacked Metrics
#'
#' @param x \code{stacked_metrics} object
#' @param ... other plot parameters
#'
#' @export
#' @rdname plot_stacked_metrics

plot.stacked_metrics <- function(x, ...){
  x <- x$expanded_data

  ggplot(x, aes(x = reorder(model, -score), y = score, fill = reorder(metric, score))) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    coord_flip() +
    theme_drwhy_vertical() +
    scale_fill_manual(values = DALEX::colors_discrete_drwhy(n=12)) +
    xlab("Model Label") +
    ylab("Cummulated metric score") +
    labs(fill = "Metric") +
    ggtitle("Stacked Metric Chart")

}
