#' Choose metric
#'
#' @description Choose metric creates \code{chosen_metric} object. It is visualization of metric data from fairness object.
#' It allows to visualize and compare it's values of chosen metric across all models.
#'
#' @param x \code{fairness_object}
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
#'


choose_metric <- function(x, fairness_metric = "FPR_parity_loss"){

  stopifnot(class(x) == "fairness_object")
  assert_parity_metrics(fairness_metric)

  data           <- cbind(x$metric_data[,fairness_metric], x$label)
  data           <- as.data.frame(data)
  colnames(data) <- c("metric", "label")
  data$metric    <- as.numeric(data$metric)


  choosen_metric <- list(metric_data = data,
                         metric = fairness_metric,
                         label  = x$label)

  class(choosen_metric) <- "chosen_metric"

  return(choosen_metric)
}













