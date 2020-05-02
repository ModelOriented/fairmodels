#' Expand Fairness Object
#'
#' @param x fairness_object
#' @param scale if true normalises metric data
#' @return
#' @export
#' @rdname expand_fairness_object
#'
#' @examples
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
#' expand_fairness_object(fobject)
#'

expand_fairness_object <- function(x, scale =FALSE){


  num_explainers <- length(x$explainers)
  m <- ncol(x$metric_data)

  metric_data <- x$metric_data[,1:(m-1)]

  if (scale) metric_data <- as.data.frame(scale(as.matrix(metric_data)))

  explainers_labels <- sapply(x$metric_data[,m], toString)

  # rows = metrics * explainers
  expanded_data <- data.frame()

  column_names <- colnames(metric_data)

  for (i in seq_len(m-1)){
    to_add <- data.frame(metric = rep(column_names[i], num_explainers),
                         model  = explainers_labels,
                         score  = metric_data[,i])
    expanded_data <- rbind(expanded_data, to_add)
  }
  expanded_data$metric <- as.factor(expanded_data$metric)
  expanded_data$model <- as.factor(expanded_data$model)
  expanded_data$score <- as.numeric(expanded_data$score)
  rownames(expanded_data) <- NULL

  return(expanded_data)
}
