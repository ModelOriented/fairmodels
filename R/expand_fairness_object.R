#' Expand fairness object
#'
#' @param x fairness object
#'
#' @return data frame more feasible to plot
#' @export
#'
#'

expand_fairness_object <- function(x){
  stopifnot(class(x) == "fairness_object")

  metric_data <- x$metric_data
  num_explainers <- length(x$explainers)
  m <- ncol(metric_data)
  explainers_labels <- sapply(metric_data[,m], toString)

  # rows = metrics * explainers
  expanded_data <- as.data.frame(matrix(0,nrow =(m-1)*num_explainers , ncol = 3))


  for (i in seq_len(m-1)){

  column_name <- colnames(metric_data)[i]
  to_add <- cbind(rep(column_name,num_explainers), explainers_labels, metric_data[,i])

    if (i == 1){
      expanded_data[1:num_explainers,] <- to_add
    } else{
      from <- num_explainers*(i-1)+1
      to <- num_explainers*i
      expanded_data[from:to,] <- to_add
    }
  }

 return(expanded_data)
}


