#' Plot fairness object
#'
#' @description Plot distribution for models output probabilities. See how being in particular subgroup affects models decision.
#'
#' @param x fairness_object
#' @param ... other fairness_objects and other parameters
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
#' rf_compas2 <- ranger(Two_yr_Recidivism ~Misdemeanor+Age_Above_FourtyFive, data = compas, probability = TRUE)
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
#' explainer_rf2 <- explain(rf_compas2, data = compas, y = y_numeric)
#' explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)
#'
#' fobject <-  fairness_check(explainer_glm, explainer_rf2,
#'                              protected = compas$Ethnicity,
#'                              privileged = "Caucasian",
#'                              label = c("lm_1","rf_2"))
#'
#' plot_density(fobject)


plot_density <- function(x, ...){

  stopifnot(class(x) == "fairness_object")

  explainers   <- x$explainers
  m            <- length(x$protected)
  density_data <- data.frame()

  for (i in seq_along(explainers)){
    tmp_data <- data.frame(probability = explainers[[i]]$y_hat,
                           label       = rep(x$label[i], m ),
                           protected   = x$protected)

    # bind with rest
    density_data <- rbind(density_data , tmp_data)
  }



  p <- ggplot(density_data, aes(probability, protected)) +
        geom_violin(color = "#ceced9", fill = "#ceced9" , alpha = 0.5) +
        geom_boxplot(aes(fill = protected) ,width = 0.3, alpha = 0.5, outlier.alpha = 0) +
        scale_x_continuous(limits = c(0,1)) +
        theme_drwhy_vertical() +
        scale_fill_manual(values = DALEX::colors_discrete_drwhy(n = m)) +
        theme(legend.position = "none", # legend off
              strip.placement = "outside",
              strip.text.y = element_text(hjust = 0.5, vjust = 1),
              ) +
        ylab("protected variable") +
        ggtitle("Density plot")
  p + facet_grid(rows = vars(label))

}




