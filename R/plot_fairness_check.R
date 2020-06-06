#' Plot fairness check
#'
#' @description Plot fairness check enables to look how big differences are between base subgroup (privileged) and unprivileged ones.
#' If barplot reaches red zone it means that for this subgroup fairness goal is not satisfied. Multiple subgroups and models can be plotted.
#' Red and green zone boundary can be moved through epsilon parameter, that needs to be passed through fairness_check.
#'
#' @param x \code{fairness_check} object
#' @param ... other \code{fairness_check} objects
#'
#' @return ggplot object
#' @export
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' # chaging levels - positive situation here is not becoming recidivist
#' compas$Two_yr_Recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
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
#'                                  data  = compas,
#'                                  group = "Sex",
#'                                  base  = "Female",
#'                                 cutoff = 0.5)
#'
#' fc <- fairness_check(fobject)
#' plot(fc)


plot.fairness_check <- function(x, ...){

  list_of_objects <- get_objects(list(x, ...), "fairness_check")
  data            <- extract_data(list_of_objects, "data")


  assert_equal_parameters(list_of_objects, "n_sub")
  assert_equal_parameters(list_of_objects, "epsilon")


  n_exp   <- length(unique(data$model))
  n_sum   <- x$n_sub
  epsilon <- x$epsilon

  upper_bound <- max(c(data$score)) + 0.05
  lower_bound <- min(c(data$score)) - 0.05

  green <- "#c7f5bf"
  red   <- "#f05a71"

  plt <- ggplot(data = data, aes(x = subgroup, y = score, fill = model)) +
    geom_rect(xmin  = -Inf,
              xmax  = Inf,
              ymin  = -epsilon,
              ymax  =  Inf,
              fill  = green,
              alpha = 0.1,) +
    geom_rect(xmin  = -Inf,
              xmax  = Inf,
              ymin  =  -Inf,
              ymax  =  -epsilon,
              fill  = red,
              alpha = 0.1,) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(lower_bound, upper_bound)) +
    coord_flip() +
    facet_wrap(vars(metric), nrow = 5) +
    theme_drwhy_vertical() +
    scale_fill_manual(values = DALEX::colors_discrete_drwhy(n = n_exp)) +
    ggtitle("Fairness check", subtitle = paste("Created with", paste(
                                               as.character(unique(x$data$model)), collapse = ", ")))

  plt
}
