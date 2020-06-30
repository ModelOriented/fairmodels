#' Print ceteris paribus cutoff
#'
#' @param x ceteris_paribus_cutoff object
#' @param ... other print parameters
#'
#' @import utils
#'
#' @export
#' @rdname print_ceteris_paribus_cutoff
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
#' ceteris_paribus_cutoff(fobject, "female")


print.ceteris_paribus_cutoff<- function(x, ...){

  data <- x$cutoff_data

  cat("\nCeteribus paribus cutoff for model:", x$subgroup, "\n")
  cat("\nFirst rows from data: \n")
  print(head(data))

  cat("\n")
  return(invisible(NULL))
}
