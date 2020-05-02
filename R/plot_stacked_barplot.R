#' Title
#'
#' @param x fairness object
#'
#' @return ggplot object
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
#' plot_stacked_barplot(fobject)
#'



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

  # other metric scores are the same, example ( TPR  = 1 - FNR ) and their parity loss is the same
  expanded_data <- expanded_data[expanded_data$metric %in% paste0(c("TPR", 'TNR', 'PPV', 'NPV','TS','ACC','F1','MCC'), "_parity_loss"),]


  plot_stacked_barplot.deafult(expanded_data)
}

#' @export
#' @rdname plot_stacked_barplot

plot_stacked_barplot.deafult <- function(x){

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
