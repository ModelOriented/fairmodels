#' Plot chosen metric
#'
#' @description Choose metric and than plot it for every model.
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
#' library(DALEX)
#' library(ranger)
#'
#' data(compas)
#'
#' rf_compas <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE) # Wszystko
#' lr_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' # numeric target values
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' # explainer
#' rf_explainer <- explain(rf_compas, data = compas[,-1], y = y_numeric)
#' lr_explainer <- explain(lr_compas, data = compas[,-1], y = y_numeric)
#'
#'
#' fobject <- create_fairness_object(rf_explainer, lr_explainer,
#'                                   data = compas,
#'                                   outcome = "Two_yr_Recidivism",
#'                                   group = "Ethnicity",
#'                                   base = "African_American")
#'
#' cm <- choose_metric(fobject, "TPR_parity_loss")
#' plot(cm)


plot.chosen_metric <- function(x, ...){

  list_of_objects <- get_objects(list(x, ...), "chosen_metric")
  data            <- extract_data(list_of_objects, "data")

  assert_equal_parameters(list_of_objects, "metric")
  fairness_metric <- x$metric

  assert_different_fairness_labels(list_of_objects)


  ggplot(data, aes(x = label, y = metric)) +
    geom_bar(stat = "identity", fill = "#46bac2", alpha = 0.8) +
    coord_flip()+
    theme_drwhy_vertical()+
    ggtitle(paste(fairness_metric, "in all models")) +
    theme(legend.position = "none") +
    xlab("models") +
    ylab(fairness_metric)
}
