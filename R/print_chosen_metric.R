#' Print chosen metric
#'
#' @param x \code{chosen_metric} object
#' @param ... other \code{chosen_metric} object
#'
#' @return
#' @export
#' @rdname print_chosen_metric
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data(compas)
#'
#' rf_compas <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE) # Wszystko
#' lr_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' # numeric target values
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' # explainer
#' rf_explainer <- explain(rf_compas, data = compas[,-1], y = y_numeric)
#' lr_explainer <- explain(lr_compas, data = compas[,-1], y = y_numeric)
#'
#'
#' fobject <- create_fairness_object(rf_explainer, lr_explainer,
#'                                   data = compas,
#'                                   outcome = "Two_yr_Recidivism",
#'                                   group = "Ethnicity",
#'                                   base = "African_American")
#'
#' cm <- choose_metric(fobject, "TPR_parity_loss")
#' print(cm)

print.chosen_metric <- function(x,...){

  list_of_objects <- get_objects(list(x, ...), "chosen_metric")
  data    <- extract_data(list_of_objects, "data")

  assert_equal_parameters(list_of_objects, "metric")

  cat("\nchoosen metric:\n", x$metric)
  cat("\ndata:\n")
  print(head(data,nrow(data)))

  cat("\n")
  return(invisible(NULL))

}
