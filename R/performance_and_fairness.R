#' Performance and fairness
#'
#' @description measure model performance and model fairness metric at the same time. Choose best model according to bot metrics.
#'
#' @description Measure performance in both fairness metric and
#'
#' @param x \code{fairness_object}
#' @param fairness_metric fairness metric, one of those in fairness_object
#' @param performance_metric performance metric, one of
#'
#' @return \code{performance_and_fairness} object
#' @export
#' @rdname performance_with_fairness
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive, data = compas, probability = TRUE)
#' lr_compas_1 <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' rf_compas_2 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' rf_compas_3 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = compas, probability = TRUE)
#' df <- compas
#' df$Two_yr_Recidivism <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_1 <- explain(rf_compas_1,  data = compas[,-1], y = y_numeric)
#' explainer_2 <- explain(lr_compas_1,  data = compas[,-1], y = y_numeric)
#' explainer_3 <- explain(rf_compas_2,  data = compas[,-1], y = y_numeric, label = "ranger_2")
#' explainer_4 <- explain(rf_compas_3,  data = compas[,-1], y = y_numeric, label = "ranger_3")
#'
#' fobject <- create_fairness_object(explainer_1,explainer_2,explainer_3,explainer_4,
#'                                     data = compas,
#'                                     outcome = "Two_yr_Recidivism",
#'                                     group  = "Ethnicity",
#'                                     base   = "Caucasian")
#' paf <- performance_and_fairness(fobject, performance_metric = "f1")
#' plot(paf)
#'



performance_and_fairness <- function(x, fairness_metric = NULL, performance_metric = NULL){
  stopifnot(class(x) == "fairness_object")

  if (is.null(fairness_metric)) {
    fairness_metric = "ACC_parity_loss"
    cat("Fairness Metric is NULL, setting deafult (", fairness_metric,")  \n")
  }

  if (is.null(performance_metric)) {
    performance_metric = "auc"
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

      mod_perf[i] <- group_model_performance(x$explainers[[i]],
                                             data = x$data,
                                             group = x$group,
                                             outcome = x$outcome,
                                             cutoff = x$cutoff,
                                             performance_metric = performance_metric)
    }
  }

  out <- as.data.frame(cbind(x$metric_data[fairness_metric],mod_perf,  x$metric_data[,ncol(x$metric_data)]))
  colnames(out) <- c("fairness_metric", "performance_metric", "labels")
  out$labels <- as.factor(out$labels)

  performance_and_fairness <- list(data = out,
                                    fairness_metric = fairness_metric,
                                    performance_metric = performance_metric,
                                    explainers = x$explainers)

  class(performance_and_fairness) <-  "performance_and_fairness"

  return(performance_and_fairness)
}
