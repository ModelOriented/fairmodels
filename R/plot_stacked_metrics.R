#' Plot stacked Metrics
#'
#' @param x \code{stacked_metrics} object
#' @param ... other \code{stacked_metrics} objects and other plot parameters
#'
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
#' explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)
#'
#' fobject <-create_fairness_object(explainer_glm, explainer_rf,
#'                              outcome = "Two_yr_Recidivism",
#'                              group   = "Ethnicity",
#'                              base    = "Caucasian",
#'                              cutoff = 0.5)
#'
#' sm <- stack_metrics(fobject)
#' plot(sm)
#'
#'
#' @export
#' @rdname plot_stacked_metrics
#'

plot.stacked_metrics <- function(x, ...){

  data <- x$expanded_data

  ggplot(data, aes(x = reorder(model, -score), y = score, fill = reorder(metric, score))) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    coord_flip() +
    theme_drwhy_vertical() +
    scale_fill_manual(values = c(DALEX::colors_discrete_drwhy(n=8),"#c295f0")) +
    xlab("Fairness Label") +
    ylab("Cummulated metric score") +
    labs(fill = "Metric") +
    ggtitle("Stacked Metric Chart")

}
