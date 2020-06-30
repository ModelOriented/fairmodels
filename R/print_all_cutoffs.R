#' Print all cutoffs
#'
#' @param x all_cuttofs object
#' @param ... other print parameters
#'
#' @export
#'
#' @import utils
#'
#' @rdname print_all_cutoffs
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
#' ac <- all_cutoffs(fobject,
#'                   label = "lm",
#'                   fairness_metrics = c("TPR_parity_loss",
#'                                        "FPR_parity_loss"))
#' print(ac)
#'

print.all_cutoffs <- function(x, ...){

  data <- x$cutoff_data

  cat("\nAll cutofs for model:\n", x$label, "\n")
  cat("\nFirst rows from data: \n")
  print(head(data))

  cat("\n")
  return(invisible(NULL))
}
