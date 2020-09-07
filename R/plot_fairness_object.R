#' Plot fairness object
#'
#' @description Plot fairness check enables to look how big differences are between base subgroup (privileged) and unprivileged ones.
#' If bar plot reaches red zone it means that for this subgroup fairness goal is not satisfied. Multiple subgroups and models can be plotted.
#' Red and green zone boundary can be moved through epsilon parameter, that needs to be passed through \code{fairness_check}.
#'
#' @param x \code{fairness_object} object
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
#'                            max.depth = 3,
#'                            num.trees = 100,
#'                            seed = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' explainer_rf <- DALEX::explain(rf_model,
#'                                data = german[,-1],
#'                                y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' plot(fobject)
#'

plot.fairness_object <- function(x, ...){

  n_exp   <- length(x$explainers)
  data    <- x$fairness_check_data
  metrics <- unique(data$metric)
  n_met   <- length(metrics)
  epsilon <- x$epsilon

  if (any(is.na(data$score))){

   warning("Omiting NA for models: ",
            paste(unique(data[is.na(data$score), "model"]),
            collapse = ", "),
           "\nInformation about passed metrics may be inaccurate due to NA present, it is advisable to check metric_scores plot.\n")
  }

  # bars should start at 0

  data$score <- data$score -1

  upper_bound <- max(na.omit(data$score), 1/epsilon -1) + 0.05
  if (upper_bound < 0.3) upper_bound <- 0.3

  lower_bound <- min(na.omit(data$score), epsilon -1 ) - 0.05
  if (lower_bound > -0.25) lower_bound <- -0.25

  green <- "#c7f5bf"
  red   <- "#f05a71"

  breaks <- seq(round(lower_bound,1), round(upper_bound,1), 0.2)
  if (! 0 %in% breaks) breaks <- round(breaks + 0.1,1)

  subgroup <- score <- model <- metric <- NULL
  plt <- ggplot(data = data, aes(x = subgroup, y = score, fill = model)) +

    # middle (green)
    annotate("rect",
              xmin  = -Inf,
              xmax  = Inf,
              ymin  =  epsilon -1 ,
              ymax  =  1/epsilon -1,
              fill  = green,
              alpha = 0.1) +
    # left (red)
    annotate("rect",
              xmin  = -Inf,
              xmax  = Inf,
              ymin  =  -Inf,
              ymax  =  epsilon -1,
              fill  = red,
              alpha = 0.1) +

    # right (red)
    annotate("rect",
              xmin  = -Inf,
              xmax  = Inf,
              ymin  =  1/epsilon -1,
              ymax  =  Inf,
              fill  = red,
              alpha = 0.1) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0) +
    coord_flip() +
    facet_wrap(vars(metric), ncol = 1) +
    scale_y_continuous(limits = c(lower_bound, upper_bound),
                       breaks =  breaks,
                       labels = breaks + 1) +
    geom_text(x = 0, y = lower_bound - 0.02, label = "bias") +
    theme_drwhy_vertical() +
    theme(panel.grid.major.y = element_blank()) +
    scale_fill_manual(values = colors_fairmodels(n_exp)) +
    ggtitle("Fairness check", subtitle = paste("Created with", paste(
                                               as.character(unique(data$model)), collapse = ", ")))

    plt


}


