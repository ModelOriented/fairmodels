#' Plot all cutoffs
#'
#'
#'
#' @param x all_cutoffs object
#' @param ... other plot parameters
#'
#' @return ggplot object
#' @export
#' @rdname plot_all_cutoffs
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
#' ac <- all_cutoffs(fobject, fairness_metrics = c("TPR_parity_loss", "F1_parity_loss"), label = "ranger")
#' plot(ac)
#'
#'

plot.all_cutoffs <- function(x, ...){

  list_of_objects <- get_objects(list(x, ...), "all_cutoffs")
  cutoff_data     <- extract_data(list_of_objects, "data")

  labels          <- lapply(list_of_objects, function(x) x$label)

  plt <- ggplot(cutoff_data, aes(cutoff, parity_loss, color = metric)) +
    geom_line() +
    theme_drwhy() +
    scale_color_manual(values = c(DALEX::colors_discrete_drwhy(n=7),"#c295f0")) +
    ggtitle("All cutoffs plot", subtitle = paste("created with", paste(labels, collapse = ", "), collapse = " ")) +
    facet_grid(rows = vars(label)) +
    xlab("value of cutoff") +
    ylab("Metric's parity loss")

  plt
}
