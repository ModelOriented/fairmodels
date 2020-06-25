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

  data <- x$fairness_check_data

  models  <- unique(data$model)
  epsilon <- x$epsilon
  metrics <- unique(data$metric)

  cat("\nFairness check for models:", paste(models, collapse = ", "), "\n")

  for (model in models){
    model_data <- data[data$model == model,]

    if (any(is.na(model_data$score))) warning("Omiting NA for model: ", model)

    failed_metrics <- unique(model_data[abs(na.omit(model_data$score)) > epsilon, "metric"])
    passed_metrics <-  length(metrics[! metrics %in% failed_metrics])

    if (passed_metrics < 4){
      cat("\n", color_codes$red_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$red_end ,  sep = "")}
    if (passed_metrics == 4){
      cat("\n", color_codes$yellow_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$yellow_end ,  sep = "")
    }
    if (passed_metrics == 5){
      cat("\n", color_codes$green_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$green_end ,  sep = "")}

    cat("Total loss: ", sum(abs(na.omit(data[data$model == model, "score" ]))), "\n")
  }

  cat("\n")
  return(invisible(NULL))

}
