#' Plot stacked Metrics
#'
#' @description Stacked metrics is like plot for \code{choosen_metric} but with all unique metrics stacked on top of each other.
#' Metrics containing NA's will be dropped to enable fair comparison.
#'
#' @param x \code{stacked_metrics} object
#' @param ... other plot parameters
#'
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
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' sm <- stack_metrics(fobject)
#' plot(sm)
#'
#' @export
#' @rdname plot_stacked_metrics
#'

plot.stacked_metrics <- function(x, ...){

  data <- x$stacked_data

  ggplot(data, aes(x = reorder(model, -score), y = score, fill = reorder(metric, score))) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    coord_flip() +
    theme_drwhy_vertical() +
    scale_fill_manual(values = colors_fairmodels(13)) +
    xlab("model") +
    ylab("Acummulated metric score") +
    labs(fill = "Metric") +
    ggtitle("Stacked Metric plot")

}