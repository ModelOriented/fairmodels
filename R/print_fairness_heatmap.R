#' Print fairness heatmap
#'
#' @param x \code{fairness_heatmap} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_fairness_heatmap
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
#'  fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = c(0.4,0.5),
#'                           label = c("lm_2", "rf_2"))
#'
#'
#' fh <- fairness_heatmap(fobject)
#' print(fh)
#'

print.fairness_heatmap <- function(x, ...) {

  data         <- x$heatmap_data
  matrix_model <- x$matrix_model

  scaled <- x$scale
  cat("heatmap data top rows: \n")
  print(head(data, 5), ...)
  cat("\n")

  cat("matrix model", ifelse(scaled, "scaled", "not scaled"), ":\n")
  print(matrix_model, ...)


  cat("\n")
}

