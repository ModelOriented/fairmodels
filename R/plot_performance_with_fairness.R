#' Plot fairness and performance
#'
#' @param x fairness object
#' @param fairness_metric name of fairness metric
#' @param performance_metric name of DALEX performance metric
#'
#'
#' @return
#' @export
#'
#' @import DALEX
#' @import ggrepel
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
#'
#' plot_performance_with_fairness(fobject, "FPR_parity_loss", "auc")
#'

plot_performance_with_fairness <- function(x , fairness_metric = NULL, performance_metric = NULL){

  if (is.null(fairness_metric)) {
    fairness_metric = "ACC_parity_loss"
    cat("Fairness Metric is NULL, setting deafult (", crayon::green(fairness_metric),")  \n")
  }

  if (is.null(performance_metric)) {
    performance_metric = "auc"
    cat("Performace metric is NULL, setting deafult (", crayon::green(performance_metric),")  \n")
  }

  # output for creating object
  cat("\nCreating object with: \nFairness metric", crayon::cyan(fairness_metric),
      "\nPerformance metric ", crayon::cyan(performance_metric), "\n")

  mod_perf <- rep(0, length(x$explainers))

  for(i in seq_along(x$explainers)){
    x$explainers[[i]]$model_info$type <- "classification" # temporary?
    mod_perf[i] <- model_performance(x$explainers[[i]], cutoff = x$cutoff)$measures[performance_metric][[1]]

  }

  out <- as.data.frame(cbind(x$metric_data[fairness_metric],mod_perf,  x$metric_data[,ncol(x$metric_data)]))
  colnames(out) <- c("fairness_metric", "performance_metric", "labels")
  out$labels <- as.factor(out$labels)


  ggplot(out, aes(x = performance_metric, y = fairness_metric)) +
    geom_text_repel(aes(label = labels),
                    segment.size  = 0.2,
                    segment.color = "grey50",
                    direction     = "x") +
    geom_point(aes(color = labels)) +
    theme_drwhy() +
    scale_color_manual(values = DALEX::colors_discrete_drwhy(n = length(x$explainers)) ) +
    ggtitle("Fairness and performance plot") +
    xlab(performance_metric) +
    ylab(fairness_metric)

}
