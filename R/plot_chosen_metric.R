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
#'
#' data("compas")
#'
#' # positive outcome - not being recidivist
#' two_yr_recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
#' y_numeric <- as.numeric(two_yr_recidivism) -1
#'
#' lm_model <- glm(Two_yr_Recidivism~.,
#'                 data=compas,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Two_yr_Recidivism ~.,
#'                            data = compas,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = compas[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = compas[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = compas$Ethnicity,
#'                           privileged = "Caucasian")
#'
#' cm <- choose_metric(fobject, "TPR_parity_loss")
#' plot(cm)


plot.chosen_metric <- function(x, ...){

  data <- x$metric_data
  fairness_metric <- x$metric


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
