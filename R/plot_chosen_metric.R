#' Plot chosen metric
#'
#' @description Choose metric from parity loss metrics and plot it for every model.
#'  The one with the least parity loss is more fair in terms of this particular metric.
#'
#' @param x object of class \code{chosen_metric}
#' @param ... other objects of class \code{chosen_metric}
#'
#' @import ggplot2
#'
#' @return \code{ggplot2} object
#' @export
#' @rdname plot_chosen_metric
#'
#' @examples
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
#' cm <- choose_metric(fobject, "TPR_parity_loss")
#' plot(cm)


plot.chosen_metric <- function(x, ...){

  data <- x$metric_data
  fairness_metric <- x$metric


  label <- metric <- NULL
  ggplot(data, aes(x = label, y = metric)) +
    geom_bar(stat = "identity", fill = "#46bac2", alpha = 0.8) +
    coord_flip()+
    theme_drwhy_vertical()+
    ggtitle("Chosen metric plot",
    subtitle = paste(fairness_metric, "in all models")) +
    theme(legend.position = "none") +
    xlab("model") +
    ylab(fairness_metric)
}
