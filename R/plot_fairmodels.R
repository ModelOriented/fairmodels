#' Plot fairmodels
#'
#' Easier access to all plots in fairmodels. Provide plot type (that matches to function name), pass additional parameters and plot.
#'
#' @param x object created with \code{fairness_check} or with \code{\link[DALEX]{explain}}
#' @param type character, type of plot. Should match function name in fairmodels. Default is fairness_check.
#' @param ... other parameters passed to fairmodels functions.
#' @param protected factor, vector containing sensitive attributes such as gender, race, etc...
#' @param privileged character/factor, level in factor denoting privileged subgroup
#'
#'
#' @details types (function names) available:
#' \itemize{
#' \item fairness_check
#' \item stack_metrics
#' \item fairness_heatmap
#' \item fairness_pca
#' \item fairness_radar
#' \item group_metric
#' \item choose_metric
#' \item metric_scores
#' \item performance_and_fairness
#' \item all_cutoffs
#' \item ceteris_paribus_cutoff
#' }
#'
#' @import ggplot2
#'
#' @return \code{ggplot2} object
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
#' explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)
#'
#' # works with explainer when protected and privileged are passed
#' plot_fairmodels(explainer_lm,
#'   type = "fairness_radar",
#'   protected = german$Sex,
#'   privileged = "male"
#' )
#'
#' # or with fairness_object
#' fobject <- fairness_check(explainer_lm,
#'   protected = german$Sex,
#'   privileged = "male"
#' )
#'
#' plot_fairmodels(fobject, type = "fairness_radar")
#' @export
#' @rdname plot_fairmodels

plot_fairmodels <- function(x, type, ...) UseMethod("plot_fairmodels")

#' @export
#' @rdname plot_fairmodels
plot_fairmodels.explainer <- function(x, type = "fairness_check", ..., protected, privileged) {
  x <- fairness_check(x, protected = protected, privileged = privileged)
  plot_fairmodels.fairness_object(x, type, ...)
}

#' @export
#' @rdname plot_fairmodels
plot_fairmodels.fairness_object <- function(x, type = "fairness_check", ...) {
  plot_fairmodels.default(x, type, ...)
}

#' @export
#' @rdname plot_fairmodels
plot_fairmodels.default <- function(x, type = "fairness_check", ...) {
  stopifnot(class(x) == "fairness_object")

  switch(type,
    fairness_check = {
      return(plot.fairness_object(x))
    },
    stack_metrics = {
      return(plot.stacked_metrics(stack_metrics(x, ...)))
    },
    fairness_heatmap = {
      return(plot.fairness_heatmap(fairness_heatmap(x, ...)))
    },
    fairness_pca = {
      return(plot.fairness_pca(fairness_pca(x, ...)))
    },
    fairness_radar = {
      return(plot.fairness_radar(fairness_radar(x, ...)))
    },
    group_metric = {
      return(plot.group_metric(group_metric(x, ...)))
    },
    choose_metric = {
      return(plot.chosen_metric(choose_metric(x, ...)))
    },
    metric_scores = {
      return(plot.metric_scores(metric_scores(x, ...)))
    },
    performance_and_fairness = {
      return(plot.performance_and_fairness(performance_and_fairness(x, ...)))
    },
    all_cutoffs = {
      return(plot.all_cutoffs(all_cutoffs(x, ...)))
    },
    ceteris_paribus_cutoff = {
      return(plot.ceteris_paribus_cutoff(ceteris_paribus_cutoff(x, ...)))
    }
  )

  stop("Type provided is not in acceptable types, please see the documentation")
}
