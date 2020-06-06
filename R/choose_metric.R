#' Choose metric
#'
#' @description choose metric and compare it's values across all models
#'
#' @param x fairness object
#' @param fairness_metric \code{char}, name of fairness metric, one of metrics:
#'
#' \itemize{
#'
#' \item TPR_parity_loss - parity loss of True Positive Rate (Sensitivity, Recall, Equal Odds)
#' \item TNR_parity_loss - parity loss of True Negative Rate (Specificity)
#' \item PPV_parity_loss - parity loss of Positive Predictive Value (Precision)
#' \item NPV_parity_loss - parity loss of Negative Predictive Value
#' \item FNR_parity_loss - parity loss of False Negative Rate
#' \item FPR_parity_loss - parity loss of False Positive Rate
#' \item FDR_parity_loss - parity loss of False Discovery Rate
#' \item FOR_parity_loss - parity loss of False Omission Rate
#' \item TS_parity_loss  - parity loss of Threat Score
#' \item ACC_parity_loss - parity loss of Accuracy
#' \item F1_parity_loss  - parity loss of F1 Score
#' \item MCC_parity_loss - parity loss of Matthews correlation coefficient
#' }
#'
#' @details some of metrics give same parity loss as others (for example TPR and FNR and that is because TPR = 1 - FNR)
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
#' cm <- choose_metric(fobject, "TPR_parity_loss")
#' plot(cm)
#'


choose_metric <- function(x, fairness_metric = "FPR_parity_loss"){
  stopifnot(class(x) == "fairness_object")
  assert_parity_metrics(fairness_metric)

  data           <- cbind(x$metric_data[,fairness_metric], x$fairness_labels)
  data           <- as.data.frame(data)
  colnames(data) <- c("metric", "label")
  data$metric    <- as.numeric(data$metric)


  choosen_metric <- list(data            = data,
                         metric          = fairness_metric,
                         fairness_labels = x$fairness_labels)

  class(choosen_metric) <- "chosen_metric"

  return(choosen_metric)
}













