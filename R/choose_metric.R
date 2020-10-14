#' Choose metric
#'
#' Extracts metrics from \code{metric_data} from fairness object.
#' It allows to visualize and compare parity loss of chosen metric values across all models.
#'
#' @param x object of class \code{fairness_object}
#' @param fairness_metric \code{char}, single name of metric, one of metrics:
#'
#' \itemize{
#'
#' \item TPR - parity loss of True Positive Rate (Sensitivity, Recall, Equal Odds)
#' \item TNR - parity loss of True Negative Rate (Specificity)
#' \item PPV - parity loss of Positive Predictive Value (Precision)
#' \item NPV - parity loss of Negative Predictive Value
#' \item FNR - parity loss of False Negative Rate
#' \item FPR - parity loss of False Positive Rate
#' \item FDR - parity loss of False Discovery Rate
#' \item FOR - parity loss of False Omission Rate
#' \item TS  - parity loss of Threat Score
#' \item ACC - parity loss of Accuracy
#' \item STP - parity loss of Statistical Parity
#' \item F1  - parity loss of F1 Score
#' }
#'
#'
#' @return \code{chosen_metric} object
#' It is a list with following fields:
#' \itemize{
#' \item{parity_loss_metric_data}{ \code{data.frame} with columns: parity_loss_metric and label}
#' \item{metric}{ chosen metric}
#' \item{label}{ character, vector of model labels}
#' }
#' @export choose_metric
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
#' cm <- choose_metric(fobject, "TPR")
#' plot(cm)
#'


choose_metric <- function(x, fairness_metric = "FPR"){

  stopifnot(class(x) == "fairness_object")
  assert_parity_metrics(fairness_metric)

  data                       <- cbind(x$parity_loss_metric_data[,fairness_metric], x$label)
  data                       <- as.data.frame(data)
  colnames(data)             <- c("parity_loss_metric", "label")
  data$parity_loss_metric    <- as.numeric(data$parity_loss_metric)


  choosen_metric <- list(parity_loss_metric_data = data,
                         metric = fairness_metric,
                         label  = x$label)

  class(choosen_metric) <- "chosen_metric"

  return(choosen_metric)
}













