#' Print chosen metric
#'
#' @description Choose metric from parity loss metrics and plot it for every model.
#' The one with the least parity loss is more fair in terms of this particular metric.
#'
#' @param x \code{chosen_metric} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_chosen_metric
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
#' cm <- choose_metric(fobject, "TPR")
#' print(cm)

print.chosen_metric <- function(x,...){

  data <- x$parity_loss_metric_data

  cat("\nchoosen metric:\n", x$metric)
  cat("\ndata:\n")
  print(data, ...)

  cat("\n")
  return(invisible(NULL))

}
