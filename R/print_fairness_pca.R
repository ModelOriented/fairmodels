#' Print fairness PCA
#'
#' @description Print principal components after using pca on fairness object
#'
#' @param x fairness object
#' @param ... other params
#'
#' @return
#'
#'
#'
#' @export
#' @rdname print_fairness_pca
#'
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism) - 1
#' # models
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~ Number_of_Priors + Age_Below_TwentyFive, data = compas, probability = TRUE)
#' rf_compas_3 <- ranger(Two_yr_Recidivism ~ Sex + Age_Above_FourtyFive, data = compas, probability = TRUE)
#' model_compas_lr <- glm(Two_yr_Recidivism ~ ., data = compas, family = binomial(link = "logit"))
#' rf_compas_5 <- ranger(Two_yr_Recidivism ~ ., data = compas, probability = TRUE)
#' rf_compas_6 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive + Misdemeanor, data = compas, probability = TRUE)
#' rf_compas_7 <- ranger(Two_yr_Recidivism ~ ., data = compas, probability = TRUE)
#' rf_compas_8 <- ranger(Two_yr_Recidivism ~ Sex + Age_Above_FourtyFive + Misdemeanor + Ethnicity, data = compas, probability = TRUE)
#'
#' # explainers
#' explainer_1 <- explain(rf_compas_1, data = compas, y = y_numeric)
#' explainer_3 <- explain(rf_compas_3, data = compas, y = y_numeric)
#' explainer_4 <- explain(model_compas_lr, data = compas, y = y_numeric)
#' explainer_5 <- explain(rf_compas_5, data = compas, y = y_numeric)
#' explainer_6 <- explain(rf_compas_6, data = compas, y = y_numeric)
#' explainer_7 <- explain(rf_compas_7, data = compas, y = y_numeric)
#'
#' # different labels
#' explainer_3$label <- "rf3"
#' explainer_4$label <- "glm"
#' explainer_5$label <- "rf5"
#' explainer_6$label <- "rf6"
#' explainer_7$label <- "rf7"
#' explainers <- list(explainer_1, explainer_3, explainer_4, explainer_5, explainer_6, explainer_7)
#'
#' fobject <- create_fairness_object(explainer_1, explainer_3, explainer_4, explainer_5, explainer_6, explainer_7,
#'   outcome = "Two_yr_Recidivism",
#'   group = "Ethnicity",
#'   base = "Caucasian"
#' )
#'
#' fpca <- fairness_pca(fobject)
#' print(fpca)


print.fairness_pca <- function(x, ...){

  cat("Fairness PCA : \n")
  print(x$x)

  cat("\nCreated with: \n")
  print(as.character(x$labels))

  cat("\nFirst two components explained", sum(x$pc_1_2)*100, "% of variance.\n")

  return(invisible(NULL))
}

