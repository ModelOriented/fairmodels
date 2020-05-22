#' Print all cutoffs
#'
#' @param x all_cuttofs object
#' @param ... other print parameters
#'
#' @return
#' @export
#'
#' @rdname print_all_cutoffs
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
#' explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)
#'
#' fobject <-create_fairness_object(explainer_glm, explainer_rf,
#'                                  outcome = "Two_yr_Recidivism",
#'                                  group = "Ethnicity",
#'                                  base = "Caucasian",
#'                                  cutoff = 0.5)
#'
#' all_cutoffs(fobject, "ranger")
#'

print.all_cutoffs <- function(x, ...){

  cat("\nAll cutofs for model:", x$explainer_label, "\n")
  cat("\nFirst rows from data: ")
  print(head(x$data))

  cat("\n")
  return(invisible(NULL))
}
