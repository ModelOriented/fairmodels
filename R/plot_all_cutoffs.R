#' Plot all cutoffs
#'
#' @description All cutoffs plot allows to check how parity loss of chosen metrics is affected by the change of cutoff. Values of cutoff
#' are the same for all subgroups (levels of protected variable) no matter what cuttof values were in fairness_object.
#'
#'
#' @param x all_cutoffs object
#' @param ... other plot parameters
#'
#' @import ggplot2
#' @importFrom DALEX theme_drwhy
#'
#' @return ggplot object
#' @export
#' @rdname plot_all_cutoffs
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
#' ac <- all_cutoffs(fobject,
#'                   label = "lm",
#'                   fairness_metrics = c("TPR_parity_loss",
#'                                        "FPR_parity_loss"))
#' plot(ac)
#'
#'

plot.all_cutoffs <- function(x, ...){

  data   <- x$cutoff_data
  label  <- unique(data$label)
  n_met  <- length(data$metric)

  cutoff <- parity_loss <- metric <- NULL
  plt <- ggplot(data, aes(cutoff, parity_loss, color = metric)) +
    geom_line() +
    theme_drwhy() +
    scale_color_manual(values = c(colors_fairmodels(n_met))) +
    ggtitle("All cutoffs plot", subtitle = paste("created with", paste(label, collapse = ", "), collapse = " ")) +
    xlab("value of cutoff") +
    ylab("Metric's parity loss")

  plt
}
