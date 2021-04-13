#' Plot stacked Metrics
#'
#' @description Stacked metrics is like plot for \code{chosen_metric} but with all unique metrics stacked on top of each other.
#' Metrics containing NA's will be dropped to enable fair comparison.
#'
#' @param x \code{stacked_metrics} object
#' @param ... other plot parameters
#'
#' @import ggplot2
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' sm <- stack_metrics(fobject)
#' plot(sm)
#'
#' \donttest{
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#' sm <- stack_metrics(fobject)
#' plot(sm)
#' }

#'
#' @export
#' @rdname plot_stacked_metrics
#' @return \code{ggplot2} object
#'

plot.stacked_metrics <- function(x, ...){

  data <- x$stacked_data
  n    <- length(unique(data$metric))

  model <- score <- metric <- NULL
  ggplot(data, aes(x    = stats::reorder(model, -score),
                   y    = score,
                   fill = stats::reorder(metric, score))) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    coord_flip() +
    theme_drwhy_vertical() +
    scale_fill_manual(values = colors_fairmodels(n)) +
    xlab("model") +
    ylab("Acummulated parity loss metrics value") +
    labs(fill = "parity loss of metrics") +
    ggtitle("Stacked Metric plot")

}
