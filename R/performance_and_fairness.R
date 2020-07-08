#' Performance and fairness
#'
#' @description measure model performance and model fairness metric at the same time. Choose best model according to both metrics. When plotted y axis is inversed to accentuate
#' that models in top right corner are the best according to both metrics.
#'
#' @description Measure performance in both fairness metric and
#'
#' @param x \code{fairness_object}
#' @param fairness_metric fairness metric, one of those in fairness_object
#' @param performance_metric performance metric, one of
#'
#' @importFrom DALEX model_performance
#'
#' @return \code{performance_and_fairness} object
#' @export
#' @rdname performance_with_fairness
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
#'  # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = list( female = 0.4),
#'                           label = c("lm_2", "rf_2"))
#'
#' paf <- performance_and_fairness(fobject)
#'
#' plot(paf)
#'



performance_and_fairness <- function(x, fairness_metric = NULL, performance_metric = NULL){
  stopifnot(class(x) == "fairness_object")

  if (is.null(fairness_metric)) {
    fairness_metric = "TPR_parity_loss"
    cat("Fairness Metric is NULL, setting deafult (", fairness_metric,")  \n")
  }

  if (is.null(performance_metric)) {
    performance_metric = "accuracy"
    cat("Performace metric is NULL, setting deafult (", performance_metric,")  \n")
  }

  # output for creating object
  cat("\nCreating object with: \nFairness metric", fairness_metric,
      "\nPerformance metric ", performance_metric, "\n")


  assert_parity_metrics(fairness_metric)
  assert_performance_metrics(performance_metric)

  mod_perf <- rep(0, length(x$explainers))

  for(i in seq_along(x$explainers)){

    # for auc get it from DALEX
    if (performance_metric == "auc"){
      mod_perf[i]  <- model_performance(x$explainers[[i]])$measures[performance_metric][[1]]

    } else {
      # if else use custom cutoff function implemented in fairmodels

      mod_perf[i] <- group_model_performance(x         = x$explainers[[i]],
                                             protected = x$protected,
                                             cutoff    = x$cutoff[[x$label[i]]],
                                             performance_metric = performance_metric)
    }
  }

  out <- as.data.frame(cbind(x$metric_data[fairness_metric],
                             mod_perf,
                             x$label))
  colnames(out) <- c("fairness_metric", "performance_metric", "labels")
  out$labels <- as.factor(out$labels)

  performance_and_fairness <- list( paf_data               = out,
                                    fairness_metric    = fairness_metric,
                                    performance_metric = performance_metric,
                                    explainers         = x$explainers,
                                    label    = x$label)

  class(performance_and_fairness) <-  "performance_and_fairness"

  return(performance_and_fairness)
}
