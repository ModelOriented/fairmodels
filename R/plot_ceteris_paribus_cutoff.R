#' Ceteris paribus cutoff plot
#'
#' @description Ceteris paribus cutoff is way to check how will parity loss behave if we changed only cutoff in one subgroup.
#' It plots object of class ceteric_paribus_cutoff. It might have two types - default and cumulated. Cumulated sums metrics and plots
#' it all in one plot. When default one is used all chosen metrics will be plotted for each model.
#'
#' @param x ceteris_paribus_cutoff object
#' @param ... other plot parameters
#'
#' @import ggplot2
#' @importFrom DALEX theme_drwhy
#'
#' @return ggplot object
#' @rdname plot_ceteris_paribus_cutoff
#' @export
#'
#' @examples
#'
#' data("compas")
#'
#' # positive outcome - not being recidivist
#' two_yr_recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
#' compas$T
#' y_numeric <- as.numeric(two_yr_recidivism) -1
#'
#' lm_model <- glm(Two_yr_Recidivism~.,
#'                 data=compas,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Two_yr_Recidivism ~.,
#'                            data = compas,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = compas[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = compas[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = compas$Ethnicity,
#'                           privileged = "Caucasian")
#'
#' cpc <- ceteris_paribus_cutoff(fobject, "African_American")
#'
#' plot(cpc)
#'


plot.ceteris_paribus_cutoff <- function(x, ...){

  data       <- x$cutoff_data
  n_models   <- length(unique(data$model))
  subgroup   <- x$subgroup
  cumulated  <- x$cumulated
  n_metrics  <- length(unique(data$metric))

  cutoff <- parity_loss <- metric <- model <- NULL
  plt <- ggplot(data)

  if (! cumulated){
    plt <- plt + geom_line(aes(cutoff, parity_loss, color = metric)) +
                      theme_drwhy() +
                      facet_wrap(vars(model), nrow = n_models)  +
                      ggtitle("Ceteris paribus cutoff plot",
                              subtitle = paste("Based on",
                                               subgroup,
                                               collapse = " ")) +
                      scale_color_manual(
                        values = colors_fairmodels(n_metrics)
                        ) +
                      xlab("value of cutoff") +
                      ylab("parity loss")


  } else {

    plt <- plt + geom_line( aes(cutoff, parity_loss, color = model)) +
                    theme_drwhy() +
                    ggtitle("Ceteris paribus cutoff plot",
                            subtitle = paste("Based on", subgroup, "and cumulated", collapse = " ")) +
                    scale_color_manual(values = colors_fairmodels(n = n_models )) +
                    xlab("value of cutoff") +
                    ylab("cummulated parity loss")


  }
  plt
}
