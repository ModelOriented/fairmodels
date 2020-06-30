#' Print stacked metrics
#'
#' @description Stack metrics sums parity loss metrics for all models. Higher value of stacked metrics means the model is less fair (has higher bias)
#' for subgroups from protected vector.
#'
#' @param x stacked_metrics object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#'
#' @rdname print_stacked_metrics
#'
#' @examples
#'
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
#' sm <- stack_metrics(fobject)
#' print(sm)


print.stacked_metrics <- function(x, ...){

  data <- x$stacked_data

  cat("\nFirst rows of stacked data: \n")
  print(head(data, ...))
  cat("\n")
}



