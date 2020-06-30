#' Print fairness radar
#'
#' @param x fairness_radar object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_fairness_radar
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
#'
#' fradar <- fairness_radar(fobject)
#'
#' print(fradar)
#'



print.fairness_radar <- function(x, ...){

  data  <- x$radar_data

  cat("\nFairness radar for: ", paste(unique(data$model), collapse = ", "), "\n")

  cat("First rows from data: \n")
  print(head(data), ...)
  cat("\n")
  return(invisible(NULL))
}
