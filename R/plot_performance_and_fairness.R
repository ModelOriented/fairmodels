#' Plot fairness and performance
#'
#' @description visualize fairness and model metric at the same time. Note that fairness metric parity scale is reversed so that the best models are in top right corner.
#'
#' @param x \code{performance_and_fairness} object
#' @param ... other plot parameters
#'
#' @return \code{ggplot} object
#' @export
#'
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#'
#' @rdname plot_performance_and_fairness
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

plot.performance_and_fairness <- function(x , ...){

  data <- x$paf_data

  performance_metric       <- x$performance_metric
  fairness_metric          <- x$fairness_metric

  n <- length(unique(data$labels))

  inversed_fairness_metric <- paste("inversed", fairness_metric, "parity loss", collapse = " " )

  ggplot(data, aes(x = performance_metric, y = fairness_metric)) +
    geom_text_repel(aes(label = labels),
                    segment.size  = 0.2,
                    segment.color = "grey50",
                    size = 4) +
    geom_point(aes(color = labels)) +
    theme_drwhy() +
    scale_color_manual(values = colors_fairmodels(n) ) +
    scale_y_reverse() +
    ggtitle("Performance and fairness plot") +
    xlab(performance_metric) +
    ylab(inversed_fairness_metric)

}
