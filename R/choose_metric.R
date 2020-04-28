#' Choose metric
#'
#' @param x fairness object
#' @param fairness_metric \code{char}, name of fairness metric, one of metric data in fairness object
#'
#' @return choose_metric object
#' @export choose_metric
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
#' choose_metric(fobject, "TPR_parity_loss")


choose_metric <- function(x, fairness_metric = "FPR_parity_loss"){
  stopifnot(class(x) == "fairness_object")

  av_metrics <- colnames(x$metric_data[,1:(ncol(x$metric_data)-1)])
  if(! fairness_metric %in%  av_metrics){
    stop("\nfairness_metric not in metrics! Check fairness_object")
  }

  data <- x$metric_data[c(fairness_metric,"label")]
  colnames(data) <- c("metric", "label")

  choosen_metric <- list(data = data, metric = fairness_metric)
  class(choosen_metric) <- "choosen_metric"

  return(choosen_metric)
}














