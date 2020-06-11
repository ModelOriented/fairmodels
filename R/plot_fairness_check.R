#' Plot fairness check
#'
#' @description Plot fairness check enables to look how big differences are between base subgroup (privileged) and unprivileged ones.
#' If barplot reaches red zone it means that for this subgroup fairness goal is not satisfied. Multiple subgroups and models can be plotted.
#' Red and green zone boundary can be moved through epsilon parameter, that needs to be passed through \code{fairness_check}. Plot can receive many objects that
#' will be plotted together with condition of equal parameters (number of subgroups, epsilon, etc...).
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
#'                                  base  = "Female")
#'
#' # changing cutoff values
#' fobject2 <-create_fairness_object(explainer_glm, explainer_rf,
#'                                  outcome = "Two_yr_Recidivism",
#'                                  data  = compas,
#'                                  group = "Sex",
#'                                  base  = "Female",
#'                                 cutoff = c(0.45,0.5),
#'                                 label = c("lm_c","rf_c")) # c for changed
#'
#' fc <- fairness_check(fobject)
#' fc2 <- fairness_check(fobject2)
#'
#' plot(fc,fc2)


plot.fairness_check <- function(x, ...){

  list_of_objects <- get_objects(list(x, ...), "fairness_check")
  data            <- extract_data(list_of_objects, "data")


  assert_equal_parameters(list_of_objects, "n_sub")
  assert_equal_parameters(list_of_objects, "epsilon")
  assert_different_label(list_of_objects)

  n_exp   <- length(unique(data$model))
  n_sub   <- x$n_sub
  n_met   <- length(unique(data$metric))
  epsilon <- x$epsilon
  metrics <- unique(x$data$metric)

  upper_bound <- max(c(data$score)) + 0.05
  if (upper_bound < 0.12) upper_bound <- 0.12

  lower_bound <- min(c(data$score)) - 0.05
  if (lower_bound > -0.12) lower_bound <- -0.12

  green <- "#c7f5bf"
  red   <- "#f05a71"

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
    theme_drwhy_vertical() +
    scale_fill_manual(values = DALEX::colors_discrete_drwhy(n = n_exp)) +
    ggtitle("Fairness check", subtitle = paste("Created with", paste(
                                               as.character(unique(data$model)), collapse = ", ")))
  plt
}
