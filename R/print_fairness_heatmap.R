#' Print fairness heatmap
#'
#' @param x \code{fairness_heatmap} object
#' @param ... other print parameters
#'
#' @export
#' @rdname print_fairness_heatmap
#'
#' @examples
#'
#' library(ranger)
#' library(DALEX)
#' data(compas)
#'
#' # models
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive, data = compas, probability = TRUE)
#' model_compas_lr <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' rf_compas_5 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' rf_compas_6 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = compas, probability = TRUE)
#' rf_compas_7 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' rf_compas_8 <- ranger(Two_yr_Recidivism ~ Sex+Age_Above_FourtyFive+Misdemeanor+Ethnicity, data = compas, probability = TRUE)
#'
#' # explainers
#' explainer_1 <- explain(rf_compas_1, data = compas, y = y_numeric)
#' explainer_4 <- explain(model_compas_lr, data = compas, y = y_numeric)
#' explainer_5 <- explain(rf_compas_5, data = compas, y = y_numeric)
#' explainer_6 <- explain(rf_compas_6, data = compas, y = y_numeric)
#' explainer_7 <- explain(rf_compas_7, data = compas, y = y_numeric)
#'
#' # different labels
#' explainer_4$label <- "glm"
#' explainer_5$label <- "rf5"
#' explainer_6$label <- "rf6"
#' explainer_7$label <- "rf7"
#'
#'
#' fobject <- create_fairness_object(explainer_1,explainer_4,explainer_5,explainer_6,explainer_7,
#'                                   outcome = "Two_yr_Recidivism",
#'                                   group  = "Ethnicity",
#'                                   base   = "Caucasian",
#'                                   cutoff = 0.5)
#'
#' fheatmap <- fairness_heatmap(fobject)
#' print(fheatmap)

print.fairness_heatmap <- function(x, ...) {
  scaled <- x$scale
  cat("heatmap data: \n")
  print(head(x$heatmap_data, 5))
  cat("\n")

  cat("matrix model", ifelse(scaled, "scaled", "not scaled"), ":\n")
  print(x$matrix_model)

}

