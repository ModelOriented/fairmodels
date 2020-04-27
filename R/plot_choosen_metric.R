#' Plot choosen metric
#'
#' @param x object of class \code{choosen_metric}
#' @param ... other plot parameters
#'
#' @import ggplot2
#'
#' @return \code{ggplot2} object
#' @export
#' @rdname plot_choosen_metric
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


plot.choosen_metric <- function(x, ...){
  fairness_metric <- x$metric
  data <- x$data

  ggplot(data, aes(x = label, y = metric, fill = label)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = DALEX::colors_discrete_drwhy(nrow(data))) +
    coord_flip()+
    theme_drwhy_vertical()+
    ggtitle(paste(fairness_metric, "in all models")) +
    theme(legend.position = "none") +
    xlab("models") +
    ylab(fairness_metric)
}
