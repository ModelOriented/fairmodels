#' Print Fairness Object
#'
#'
#' @param x \code{fairness object}
#' @param ... other parameters
#' @param colorize logical, whether information about metrics should be in color or not
#'
#' @importFrom utils head
#' @importFrom stats na.omit
#'
#' @export
#'
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' print(fobject)
#'


print.fairness_object <- function(x, ..., colorize = TRUE){

  if (colorize) {
    color_codes <- list(yellow_start = "", yellow_end = "",
                        red_start = "", red_end = "",
                        green_start = "", green_end = "")
  }


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
