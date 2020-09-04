#' Stack metrics
#'
#' Stack metrics sums parity loss metrics for all models. Higher value of stacked metrics means the model is less fair (has higher bias)
#' for subgroups from protected vector.
#'
#' @param x object of class \code{fairness_object}
#' @param fairness_metrics character, vector of fairness metric names to include in plot. Default are ones in fairness check.
#'
#' @return \code{stacked_metrics} object. It contains \code{data.frame} with information about score for each metric and model.
#'
#' @export
#'
#' @import ggplot2
#'
#' @rdname plot_stacked_barplot
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
#' sm <- stack_metrics(fobject)
#' plot(sm)



stack_metrics <- function(x, fairness_metrics = fairness_check_metrics() ){

  stopifnot(class(x) == "fairness_object")

  if (is.null(fairness_metrics)) fairness_metrics <- fairness_check_metrics()
  if (! is.character(fairness_metrics) ) stop("metric argument must be character metric")
  sapply(fairness_metrics,assert_parity_metrics)

  expanded_data <- expand_fairness_object(x,  drop_metrics_with_na = TRUE)

  expanded_data           <- as.data.frame(expanded_data)
  colnames(expanded_data) <- c("metric","model","score")
  expanded_data$metric    <- as.factor(expanded_data$metric)
  expanded_data$model     <- as.factor(expanded_data$model)
  expanded_data$score     <- round(as.numeric(expanded_data$score),3)

  expanded_data <- expanded_data[expanded_data$metric %in% fairness_metrics,]


  stacked_metrics <- list(stacked_data = expanded_data)
  class(stacked_metrics) <- "stacked_metrics"

  return(stacked_metrics)
}


