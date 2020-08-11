#' Plot fairness object
#'
#' @description Plot fairness check enables to look how big differences are between base subgroup (privileged) and unprivileged ones.
#' If barplot reaches red zone it means that for this subgroup fairness goal is not satisfied. Multiple subgroups and models can be plotted.
#' Red and green zone boundary can be moved through epsilon parameter, that needs to be passed through \code{fairness_check}.
#'
#' @param x \code{fairness_check} object
#' @param ... other plot parameters
#'
#' @import ggplot2
#' @importFrom DALEX theme_drwhy_vertical
#'
#' @return \code{ggplot2} object
#' @rdname plot_fairness_object
#' @export
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
#' plot(fobject)

plot.fairness_object <- function(x, ...){

  n_exp   <- length(x$explainers)
  data    <- x$fairness_check_data
  metrics <- unique(data$metric)
  n_met   <- length(metrics)
  epsilon <- x$epsilon

  if (any(is.na(data$score))){

   warning("Omiting NA for models: ",
            paste(unique(data[is.na(data$score), "model"]),
            collapse = ", "))
  }

  upper_bound <- max(c(na.omit(data$score))) + 0.05
  if (upper_bound < 0.12) upper_bound <- 0.12

  lower_bound <- min(c(na.omit(data$score))) - 0.05
  if (lower_bound > -0.12) lower_bound <- -0.12

  green <- "#c7f5bf"
  red   <- "#f05a71"

  subgroup <- score <- model <- metric <- NULL
  plt <- ggplot(data = data, aes(x = subgroup, y = score, fill = model)) +

    # middle (green)
    annotate("rect",
              xmin  = -Inf,
              xmax  = Inf,
              ymin  = -epsilon,
              ymax  =  epsilon,
              fill  = green,
              alpha = 0.1) +
    # left (red)
    annotate("rect",
              xmin  = -Inf,
              xmax  = Inf,
              ymin  =  -Inf,
              ymax  =  -epsilon,
              fill  = red,
              alpha = 0.1) +

    # right (red)
    annotate("rect",
              xmin  = -Inf,
              xmax  = Inf,
              ymin  =  epsilon,
              ymax  =  Inf,
              fill  = red,
              alpha = 0.1) +

    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(lower_bound, upper_bound)) +
    coord_flip() +
    facet_wrap(vars(metric), ncol = 1) +
    geom_text(x = 0, y = lower_bound - 0.02, label = "bias") +
    theme_drwhy_vertical() +
    scale_fill_manual(values = colors_fairmodels(n_exp)) +
    ggtitle("Fairness check", subtitle = paste("Created with", paste(
                                               as.character(unique(data$model)), collapse = ", ")))
  plt
}


