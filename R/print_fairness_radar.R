#' Print fairness radar
#'
#' @param x fairness_radar object
#' @param ... other print parameters
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
#' fairness_radar(fobject)



print.fairness_radar <- function(x, ...){

  list_of_objects   <- get_objects(list(x, ...), "fairness_radar")
  data_list         <- lapply(list_of_objects, function(x) x$df)
  df                <- do.call("rbind", data_list)


  cat("\nFairness radar for: ", paste(unique(df$model), collapse = ", "), "\n")

  cat("First rows from data: \n")
  print(head(df))
  cat("\n")
  return(invisible(NULL))
}
