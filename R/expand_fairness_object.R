#' Expand Fairness Object
#'
#' @description Unfold fairness object to 3 columns (metrics, label, score) to construct better base for visualization.
#'
#' @param x fairness_object
#' @param drop_metrics_with_na logical, if TRUE metrics with NA will be omitted
#' @param scale logical, if \code{TRUE} standarised.
#'
#' @export
#' @rdname expand_fairness_object
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
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
