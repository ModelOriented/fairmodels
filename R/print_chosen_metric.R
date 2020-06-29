#' Print chosen metric
#'
#' @description Choose metric from parity loss metrics and plot it for every model.
#' The one with the least parity loss is more fair in terms of this particular metric.
#'
#' @param x \code{chosen_metric} object
#' @param ... other \code{chosen_metric} object
#'
#' @return
#' @export
#' @rdname print_chosen_metric
#' @examples
#'
#' data("compas")
#'
#' # positive outcome - not being recidivist
#' two_yr_recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
#' y_numeric <- as.numeric(two_yr_recidivism) -1
#'
#' lm_model <- glm(Two_yr_Recidivism~.,
#'                 data=compas,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Two_yr_Recidivism ~.,
#'                            data = compas,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = compas[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = compas[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = compas$Ethnicity,
#'                           privileged = "Caucasian")
#'
#' cm <- choose_metric(fobject, "TPR_parity_loss")
#' print(cm)

print.chosen_metric <- function(x,...){

  data <- x$metric_data

  cat("\nchoosen metric:\n", x$metric)
  cat("\ndata:\n")
  print(head(data,nrow(data)))

  cat("\n")
  return(invisible(NULL))

}
