#' Plot group metric
#'
#' @description Plot chosen metric in group. Notice how models are treating different subgroups.
#' Compare models both in fairness metrics and in performance. Parity loss can be enabled when creating \code{group_metric} object.
#'
#' @param x object of class group_metric
#' @param ... other group_metric objects and other parameters
#'
#' @import ggplot2
#'
#' @return list of \code{ggplot2} objects
#' @export
#'
#' @rdname plot_group_metric
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) - 1
#'
#' lm_model <- glm(Risk ~ .,
#'   data = german,
#'   family = binomial(link = "logit")
#' )
#'
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'   protected = german$Sex,
#'   privileged = "male"
#' )
#'
#' gm <- group_metric(fobject, "TPR", "f1", parity_loss = TRUE)
#' plot(gm)
#' \donttest{
#'
#' rf_model <- ranger::ranger(Risk ~ .,
#'   data = german,
#'   probability = TRUE,
#'   num.trees = 200
#' )
#'
#' explainer_rf <- DALEX::explain(rf_model, data = german[, -1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#' gm <- group_metric(fobject, "TPR", "f1", parity_loss = TRUE)
#'
#' plot(gm)
#' }
#'
plot.group_metric <- function(x, ...) {
  data <- x$group_metric_data
  performance_data <- x$performance_data

  fairness_metric <- x$fairness_metric
  performance_metric <- x$performance_metric

  # extracting number of labels
  n <- length(unique(data$model))

  # global variables
  model <- group <- score <- label <- NULL

  plot1 <- ggplot(data, aes(x = group, y = score, fill = model)) +
    geom_bar(
      stat = "identity",
      position = "dodge"
    ) +
    DALEX::theme_drwhy() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "none"
    ) +
    ylab(fairness_metric) +
    xlab("subgroups") +
    scale_fill_manual(values = colors_fairmodels(n)) +
    ggtitle("Group metric plot")

  plot2 <- ggplot(performance_data, aes(x = model, y = score, fill = model)) +
    geom_bar(
      stat = "identity",
      width = 0.4
    ) +
    geom_text(aes(label = round(score, 3)),
      vjust = -1,
      color = "black",
      size = 3,
      fontface = "bold"
    ) +
    DALEX::theme_drwhy() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 0.3)
    ) +
    scale_fill_manual(values = colors_fairmodels(n)) +
    scale_y_continuous(limits = c(0, 1)) +
    xlab("Models") +
    ylab(performance_metric)
  plot1 + plot2
}
