#' Plot all cutoffs
#'
#' All cutoffs plot allows to check how parity loss of chosen metrics is affected by the change of cutoff. Values of cutoff
#' are the same for all subgroups (levels of protected variable) no matter what cutoff values were in \code{fairness_object}.
#'
#'
#' @param x \code{all_cutoffs} object
#' @param ... other plot parameters
#' @param label character, label of model to plot. Default NULL. If default prints all models.
#'
#' @import ggplot2
#'
#' @return \code{ggplot2} object
#' @export
#' @rdname plot_all_cutoffs
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
#' explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'   protected = german$Sex,
#'   privileged = "male"
#' )
#'
#' ac <- all_cutoffs(fobject)
#' plot(ac)
#' \donttest{
#' rf_model <- ranger::ranger(Risk ~ .,
#'   data = german,
#'   probability = TRUE,
#'   num.trees = 100,
#'   seed = 1
#' )
#'
#'
#' explainer_rf <- DALEX::explain(rf_model,
#'   data = german[, -1],
#'   y = y_numeric
#' )
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#' ac <- all_cutoffs(fobject)
#' plot(ac)
#' }
#'
plot.all_cutoffs <- function(x, ..., label = NULL) {
  if (is.null(label)) {
    data <- x$cutoff_data
  } else {
    if (!is.character(label) | length(label) > 1) stop("label must be character")
    data <- x$cutoff_data[x$cutoff_data$label == label, ]
  }

  label <- unique(data$label)
  n_met <- length(unique(data$metric))

  cutoff <- parity_loss <- metric <- NULL
  plt <- ggplot(data, aes(cutoff, parity_loss, color = metric)) +
    geom_line() +
    DALEX::theme_drwhy() +
    scale_color_manual(values = c(colors_fairmodels(n_met))) +
    labs(color = "parity loss metric") +
    ggtitle("All cutoffs plot", subtitle = paste("created with", paste(label, collapse = ", "), collapse = " ")) +
    facet_wrap(vars(label), nrow = length(label)) +
    xlab("value of cutoff") +
    ylab("Metric's parity loss")

  plt
}
