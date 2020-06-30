#' Print group metric
#'
#' @param x \code{group_metric} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_group_metric
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
#' gm <- group_metric(fobject, "TPR", "f1", parity_loss = TRUE)
#'
#' print(gm)

print.group_metric <- function(x, ...){


  cat("Fairness data top rows for", x$fairness_metric, "\n")
  print(head(x$group_metric_data, ...))
  cat("\n")

  cat("Performance data for", x$performance_metric, ":")

  perf_df <- x$performance_data
  colnames(perf_df) <- NULL
  print(perf_df)

  cat("\n")
  return(invisible(NULL))
}
