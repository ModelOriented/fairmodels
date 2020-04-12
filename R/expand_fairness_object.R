#' Expand Fairness Object
#'
#' @param x fairness_object
#' @param scale if true normalises metric data
#' @return
#' @export
#' @rdname expand_fairness_object

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

  return(expanded_data)
}
