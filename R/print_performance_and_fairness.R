#' Print performance and fairness
#'
#' @param x \code{performance_and_fairness} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_performance_and_fairness
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
#'  # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = list(female = 0.4),
#'                           label = c("lm_2", "rf_2"))
#'
#' paf <- performance_and_fairness(fobject)
#'
#' paf

print.performance_and_fairness <- function(x, ...){

  data               <- x$paf_data
  performance_metric <- x$performance_metric
  fairness_metric    <- x$fairness_metric

  cat("performance_and_fairness object created for: \n")
  print(x$label)

  cat("\ndata: \n")
  print(data, ...)

  return(invisible(NULL))

}

