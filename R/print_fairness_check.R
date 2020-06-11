#' Print fairness check
#'
#' @param x fairness_check object
#' @param ... other plot parameters
#'
#' @return
#' @export
#'
#' @rdname print_fairness_check
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' # chaging levels - positive situation here is not becoming recidivist
#' compas$Two_yr_Recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
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
#'                                  data  = compas,
#'                                  group = "Sex",
#'                                  base  = "Female",
#'                                 cutoff = 0.5)
#'
#' fairness_check(fobject)
#'
#' fobject2 <-create_fairness_object(explainer_glm, explainer_rf,
#'                                  outcome = "Two_yr_Recidivism",
#'                                  data  = compas,
#'                                  group = "Sex",
#'                                  base  = "Female",
#'                                  cutoff = c(0.5,0.6) )
#' fairness_check(fobject2)
#'

print.fairness_check <- function(x, ...){

  list_of_objects <- get_objects(list(x, ...), "fairness_check")
  data            <- extract_data(list_of_objects, "data")


  assert_equal_parameters(list_of_objects, "n_sub")
  assert_equal_parameters(list_of_objects, "epsilon")
  assert_different_label(list_of_objects)

  models  <- unique(data$model)
  epsilon <- x$epsilon
  metrics <- unique(data$metric)

  cat("\nFairness check for models:", paste(models, collapse = ", "), "\n")

  for (model in models){
    model_data <- data[data$model == model,]
    failed_metrics <- unique(model_data[abs(model_data$score) > epsilon, "metric"])
    passed_metrics <-  length(metrics[! metrics %in% failed_metrics])

    if (passed_metrics < 4){
      cat("\n", color_codes$red_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$red_end ,  sep = "")}
    if (passed_metrics == 4){
      cat("\n", color_codes$yellow_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$yellow_end ,  sep = "")
    }
    if (passed_metrics == 5){
      cat("\n", color_codes$green_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$green_end ,  sep = "")}

    cat("Total loss: ", sum(abs(data[data$model == model, "score" ])), "\n")
  }

  cat("\n")
  return(invisible(NULL))
}

color_codes <- list(yellow_start = "\033[33m", yellow_end = "\033[39m",
                    red_start = "\033[31m", red_end = "\033[39m",
                    green_start = "\033[32m", green_end = "\033[39m")
