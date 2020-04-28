#' Plot fairness and performance
#'
#' @param x fairness object
#' @param ... other plot parameters
#'
#'
#' @return
#' @export
#'
#' @import DALEX
#' @import ggrepel
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive, data = compas, probability = TRUE)
#' lr_compas_1 <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' rf_compas_2 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' rf_compas_3 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = compas, probability = TRUE)
#' df <- compas
#' df$Two_yr_Recidivism <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_1 <- explain(rf_compas_1,  data = compas[,-1], y = y_numeric)
#' explainer_2 <- explain(lr_compas_1,  data = compas[,-1], y = y_numeric)
#' explainer_3 <- explain(rf_compas_2,  data = compas[,-1], y = y_numeric, label = "ranger_2")
#' explainer_4 <- explain(rf_compas_3,  data = compas[,-1], y = y_numeric, label = "ranger_3")
#'
#' fobject <- create_fairness_object(explainer_1,explainer_2,explainer_3,explainer_4,
#'                                     data = compas,
#'                                     outcome = "Two_yr_Recidivism",
#'                                     group  = "Ethnicity",
#'                                     base   = "Caucasian")
#'
#'
#' plot(performance_with_fairness(fobject))

plot.performance_with_fairness <- function(x , ...){

  data <- x$data
  performance_metric <- x$performance_metric
  fairness_metric <- x$fairness_metric


  ggplot(data, aes(x = performance_metric, y = fairness_metric)) +
    geom_text_repel(aes(label = labels),
                    segment.size  = 0.2,
                    segment.color = "grey50",
                    direction     = "x") +
    geom_point(aes(color = labels)) +
    theme_drwhy() +
    scale_color_manual(values = DALEX::colors_discrete_drwhy(n = length(x$explainers)) ) +
    scale_y_reverse() +
    ggtitle("Fairness and performance plot") +
    xlab(performance_metric) +
    ylab(fairness_metric)

}
