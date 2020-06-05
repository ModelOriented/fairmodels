#' Print stacked metrics
#'
#' @param x stacked_metrics object
#' @param ... other stacked_metrics objects and print params
#'
#' @return
#' @export
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
#' stack_metrics(fobject)


print.stacked_metrics <- function(x, ...){

  list_of_objects   <- get_objects(list(x, ...), "stacked_metrics")
  data              <- extract_data(list_of_objects, "expanded_data")

  cat("\nFirst rows of stacked data: \n")
  print(head(data))
  cat("\n")
}



