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
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'                           protected = german$Sex,
#'                           privileged = "male")
#' plot(fobject)
#'
#' \donttest{
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            max.depth = 3,
#'                            num.trees = 100,
#'                            seed = 1)
#'
#'
#' explainer_rf <- DALEX::explain(rf_model,
#'                                data = german[,-1],
#'                                y = y_numeric)
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#' plot(fobject)
#'}
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

  #### first the visible values and breaks ####

  upper_bound <- max(na.omit(data$score), 1/epsilon -1) + 0.05
  if (upper_bound < 1.3) upper_bound <- 1.3

  lower_bound <- min(na.omit(data$score), epsilon ) - 0.05
  if (lower_bound > 0.75) lower_bound <- 0.75

  green <- "#c7f5bf"
  red   <- "#f05a71"

  ticks <- get_nice_ticks(lower_bound, upper_bound)

  breaks <- seq(ticks$min, ticks$max, ticks$spacing)

  if (! 1 %in% breaks) breaks <- c(breaks, 1)

  breaks <- breaks[breaks >= lower_bound & breaks <= upper_bound]

  #### now the 'backend' values for plots ####

  # bars should start at 0
  data$score <- data$score - 1

  upper_bound <- max(na.omit(data$score), 1/epsilon -1) * 1.05
  if (upper_bound < 0.3) upper_bound <- 0.3

  lower_bound <- min(na.omit(data$score), epsilon -1 ) * 1.1
  if (lower_bound > -0.25) lower_bound <- -0.25

  #### plotting ####

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
                       breaks =  breaks - 1,
                       labels = breaks,
                       expand = c(0, 0),
                       minor_breaks = NULL) +
    theme_drwhy_vertical() +
    theme(panel.grid.major.y = element_blank()) +
    scale_fill_manual(values = colors_fairmodels(n_exp)) +
    ggtitle("Fairness check", subtitle = paste("Created with", paste(
                                               as.character(unique(data$model)), collapse = ", ")))
    ####
    plt

}






