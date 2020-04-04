#' Print Two models object
#'
#' @param x two_models object
#' @param ... other parameters
#'
#' @return
#' @export
#' @rdname print_two_models
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' rf_1 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' lr_1 <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' explainer_1 <- explain(rf_1, data = compas, y = y_numeric)
#' explainer_2 <- explain(lr_1, data = compas, y = y_numeric)
#'
#' two_m <- two_models(explainer_1, explainer_2,
#'                     fairness_metric = "fpr_parity",
#'                     outcome = "Two_yr_Recidivism",
#'                     group  = "Ethnicity",
#'                     base   = "Caucasian")
#'
#' print(two_m)

print.two_models <- function(x,...){

  cat("two_models object based on", crayon::green(x$labels[1]), "and", crayon::green(x$labels[2]), "\n\n")
  cat("Fairness metric: ", x$fairness_metric, "\n")
  cat("Performance metric: ", x$performance_metric, "\n")

  cat("Fairness metric measurements: \n")
  print(x$fairness)

  cat("Model performance measurements: \n")
  print(x$performance)
}
