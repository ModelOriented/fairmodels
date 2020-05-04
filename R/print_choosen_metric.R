#' Print choosen metric
#'
#' @param x \code{choosen_metric} object
#' @param ... other print parameters
#'
#' @return
#' @export
#' @rdname print_choosen_metric
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

print.choosen_metric <- function(x,...){
  cat("\nchoosen metric:\n", x$metric)
  cat("\ndata:\n")
  print(head(x$data,nrow(x$data)))
  return(invisible(NULL))
}
