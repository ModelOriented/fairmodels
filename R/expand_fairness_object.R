#' Expand Fairness Object
#'
#' @description Unfold fairness object to 3 columns (metrics, label, score) to construct better base for visualization.
#'
#' @param x fairness_object
#' @param drop_metrics_with_na logical, if TRUE metrics with NA will be omitted
#' @param scale logical, if \code{TRUE} standarised.
#'
#' @return
#' @export
#' @rdname expand_fairness_object
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
#'                              cutoff = 0.5)
#'
#' expand_fairness_object(fobject, drop_metrics_with_na = TRUE)
#'

expand_fairness_object <- function(x, scale = FALSE, drop_metrics_with_na = FALSE){

  stopifnot(is.logical(scale))
  stopifnot(is.logical(drop_metrics_with_na))
  stopifnot(class(x) == "fairness_object")

  n_exp          <- length(x$explainers)
  metric_data    <- x$metric_data
  labels         <- x$label

  if (drop_metrics_with_na) {
    metric_data <- drop_metrics_with_na(metric_data)
  }

  if (scale) metric_data <- as.data.frame(scale(as.matrix(metric_data)))


  # rows = metrics * explainers
  expanded_data <- data.frame()
  column_names <- colnames(metric_data)

  n_metrics <- ncol(metric_data)

  for (i in seq_len(n_metrics)){
    to_add <- data.frame(metric = rep(column_names[i], n_exp),
                         model  = labels,
                         score  = metric_data[,i])
    expanded_data <- rbind(expanded_data, to_add)
  }

  expanded_data$metric <- as.factor(expanded_data$metric)
  expanded_data$model <- as.factor(expanded_data$model)
  expanded_data$score <- as.numeric(expanded_data$score)
  rownames(expanded_data) <- NULL

  return(expanded_data)
}
