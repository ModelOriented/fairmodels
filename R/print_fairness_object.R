#' Print Fairness Object
#'
#'
#' @param x fairness object
#' @param ... other parameters
#'
#' @export
#'
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
#'                              outcome = "Two_yr_Recidivism",
#'                              group = "Ethnicity",
#'                              base = "Caucasian",
#'                              cutoff = 0.5)
#'
#' print(fobject)
#'


print.fairness_object <- function(x, ...){

  objects   <- get_objects(list(x, ...), "fairness_object")
  data_list <- lapply(objects, function(x) x$metric_data)
  data      <- do.call("rbind", data_list)

  cat("Fairness Matrics: \n")
  print(data)
  cat("Models explained (fairness labels):\n")

  for (label in x$label) print(label)

  cat("\nData: \n")
  print(head(x$data,2))

  cat("\n")
  return(invisible(NULL))
}
