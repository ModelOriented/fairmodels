#' Ceteris paribus cutoff plot
#'
#' @param x ceteris_paribus_cutoff object
#' @param ... other ceteris_paribus_cutoff objects
#'
#' @return ggplot object
#' @rdname plot_ceteris_paribus_cutoff
#' @export
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
#' explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)
#'
#' fobject <-create_fairness_object(explainer_glm, explainer_rf,
#'                                  outcome = "Two_yr_Recidivism",
#'                                  group = "Ethnicity",
#'                                  base = "Caucasian",
#'                                  cutoff = 0.5)
#'
#' cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American")
#' plot(cpc)
#'
#'
#' cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American",
#'                               cumulated = TRUE,
#'                               fairness_metrics = c("TPR_parity_loss","PPV_parity_loss", "TNR_parity_loss" ))
#' plot(cpc)
#'
#'


plot.ceteris_paribus_cutoff <- function(x, ...){

  n_models   <- length(unique(data$model))
  subgroup   <- x$subgroup
  cumulated  <- x$cumulated
  n_metrics  <- length(unique(data$metric))

  plt <- ggplot(data)

  if (! cumulated){
    plt <- plt + geom_line(aes(cutoff, parity_loss, color = metric)) +
                      theme_drwhy() +
                      facet_wrap(vars(model), nrow = n_models)  +
                      ggtitle("Ceteris paribus cutoff plot",
                              subtitle = paste("Based on", subgroup, collapse = " ")) +
                      scale_color_manual(values = DALEX::colors_discrete_drwhy(n = n_metrics )) +
                      xlab("value of cutoff") +
                      ylab("parity loss")


  } else {

    plt <- plt + geom_line( aes(cutoff, parity_loss, color = model)) +
                    theme_drwhy() +
                    ggtitle("Ceteris paribus cutoff plot",
                            subtitle = paste("Based on", subgroup, "and cumulated", collapse = " ")) +
                    scale_color_manual(values = DALEX::colors_discrete_drwhy(n = n_models )) +
                    xlab("value of cutoff") +
                    ylab("cummulated parity loss")


  }
  plt
}
