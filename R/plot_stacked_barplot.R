#' Stack metrics
#'
#' @description Stacked metrics is like plot for \code{choosen_metric} but with all unique metrics stacked on top of each other. Metrics containing NA's will be dropped to enable fair comparison.
#'
#' @param x \code{fairness_object}
#'
#' @return \code{stacked_metrics} object
#' @export
#'
#' @import ggplot2
#' @import DALEX
#'
#' @rdname plot_stacked_barplot
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
#'                              group = "Ethnicity",
#'                              base = "Caucasian",
#'                              cutoff = 0.5)
#'
#' sm <- stack_metrics(fobject)
#' plot(sm)



stack_metrics <- function(x){

  stopifnot(class(x) == "fairness_object")

  expanded_data <- expand_fairness_object(x,  drop_metrics_with_na = TRUE)

  expanded_data           <- as.data.frame(expanded_data)
  colnames(expanded_data) <- c("metric","model","score")
  expanded_data$metric    <- as.factor(expanded_data$metric)
  expanded_data$model     <- as.factor(expanded_data$model)
  expanded_data$score     <- round(as.numeric(expanded_data$score),3)

  # other metric scores are the same, example ( TPR  = 1 - FNR ) and their parity loss is the same
  expanded_data <- expanded_data[expanded_data$metric %in% unique_metrics(),]


  stacked_metrics <- list(expanded_data = expanded_data, l = FALSE)
  class(stacked_metrics) <- "stacked_metrics"

  return(stacked_metrics)
}

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
    labs(fill = "Model Label") +
    ggtitle("Stacked Metric Chart")

}
