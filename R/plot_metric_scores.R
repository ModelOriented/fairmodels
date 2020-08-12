#' Plot metric scores
#'
#' @param x \code{metric_scores} object
#' @param ... other plot parameters
#'
#' @return \code{ggplot2} object
#' @export
#' @rdname plot_metric_scores
#'
#' @examples
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
#' ms <- metric_scores(fobject, fairness_metrics = c("TPR","STP","ACC"))
#' plot(ms)
#'

plot.metric_scores <- function(x, ...){

  data <- x$metric_scores_data

  for (i in as.numeric(unique(data$model))){
    n_subgroups <- length(unique(data$subgroup))
    border <- 1/(n_subgroups + 2)
    data[data$model == levels(data$model)[i],"model_jitter"] <- rep(seq(i - 0.5 + border, i + 0.5 - border, length.out = n_subgroups), length(unique(data$metric)))
  }


  #data <- data$score[data$subgroup == x$privileged]
  data$line_position <- rep(data[data$subgroup == x$privileged, "score"], each = length(unique(data$subgroup)))
  data <- data[data$subgroup != x$privileged,]
  data$model_numeric <- as.numeric(data$model)

  score <- line_position <- model_jitter <- model_numeric <- model <- subgroup <- NULL
  ggplot() +
    geom_segment(data = data,
                 aes(x = score, xend = line_position, y = model_jitter , yend = model_jitter, color = model),
                 alpha = 0.3) +
    geom_segment(data = data,
                 aes(x = line_position, xend = line_position, y = model_numeric - 0.5, yend = model_numeric + 0.5, color = model  )) +
    geom_point(data = data,
               aes(x = score, y = model_jitter, color = model, shape = subgroup, group = subgroup),
               size = 2.5) +
    facet_wrap(~metric,
               nrow = length(unique(data$metric))) +
    scale_y_continuous("Model",
                       breaks = unique(data$model_numeric),
                       labels = levels(data$model)) +
    theme_drwhy_vertical() +
    ggtitle("Metric scores plot",
            subtitle = paste("Created with", paste(
      as.character(unique(data$model)), collapse = ", ")))

}
