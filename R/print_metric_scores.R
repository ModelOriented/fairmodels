#' Print metric scores data
#'
#' @param x \code{metric_scores} object
#' @param ... other print parameters
#'
#' @export
#' @rdname print_metric_scores
#'
#' @examples
#'
#' data("compas")
#'
#' # flipping outcomes, 1- favorable
#' compas$Two_yr_Recidivism <- as.factor(ifelse(compas$Two_yr_Recidivism == '1', '0', '1'))
#'
#' # train
#' lm_compas <- glm(Two_yr_Recidivism ~., data = compas, family = binomial())
#' rf_compas <- ranger::ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#'
#' # numeric target values
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' # explainer
#' rf_explainer <- DALEX::explain(rf_compas, data = compas[,-1], y = y_numeric)
#' lm_explainer <- DALEX::explain(lm_compas, data = compas[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(rf_explainer, lm_explainer,
#'                           protected = compas$Ethnicity,
#'                           privileged = "Caucasian")
#'
#' ms <- metric_scores(fobject, fairness_metrics = c("TPR","STP","ACC"))
#' ms

print.metric_scores <- function(x, ...){
  data  <- x$metric_scores_data

  cat("\nMetric scores calculated for: ", paste(unique(data$model), collapse = ", "), "\n")

  cat("First rows from data: \n")
  print(head(data), ...)
  cat("\n")
  return(invisible(NULL))
}

