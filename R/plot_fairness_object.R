#' Plot fairness object
#'
#' @description Plot distribution for models output probabilities. See how being in particular subgroup affects models decision.
#'
#' @param x fairness_object
#' @param ... other parameters
#'
#' @return \code{ggplot} object
#' @export
#' @rdname plot_fairness_object
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
#'                              outcome = "Two_yr_Recidivism",
#'                              group = "Ethnicity",
#'                              base = "Caucasian",
#'                              cutoff = 0.4)
#' plot(fobject)

plot.fairness_object <- function(x, ...){

  explainers <- x$explainers
  data       <- x$data
  n          <- nrow(data)

  data_combined <- data.frame()

  for (i in seq_along(explainers)){
    data$`_probability_` <- explainers[[i]]$y_hat
    data$`_label_`       <- rep(explainers[[i]]$label, n )

    # bind with rest
    data_combined <- rbind(data_combined , data)
  }

  # rename columns
  to_change <- match(TRUE , colnames(data_combined) == x$group)
  colnames(data_combined)[to_change] <- "group"


  p <- ggplot(data_combined, aes(`_probability_`, group)) +
        geom_violin(color = "#ceced9", fill = "#ceced9" , alpha = 0.5) +
        geom_boxplot(aes(fill = group) ,width = 0.3, alpha = 0.5, outlier.alpha = 0) +
        scale_x_continuous(limits = c(0,1)) +
        theme_drwhy_vertical() +
        scale_fill_manual(values = DALEX::colors_discrete_drwhy(n = n)) +
        theme(legend.position = "none", # legend off
              strip.placement = "outside",
              strip.text.y = element_text(hjust = 0.5, vjust = 1),
              ) +
        ylab(x$group) +
        ggtitle("Probability plot")
  p + facet_grid(rows = vars(`_label_`))

}


theme_bw()


