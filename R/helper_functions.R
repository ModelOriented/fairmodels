### HELPER FUNCTIONS ###


unique_metrics <- function(){
  out <- paste0(c("TPR", 'TNR', 'PPV', 'NPV','TS','ACC','F1','MCC'), "_parity_loss")
  return(out)
}


drop_metrics_with_na <- function(data){
na_col_index <- apply(data, 2,  function(x) any(is.na(x)))
cols_with_missing <- names(data)[na_col_index]

if (any(na_col_index)) warning("Found metric with NA: ", paste(cols_with_missing, collapse = ", "), ", omiting it\n")

data <- data[, !na_col_index]
return(data)
}

assert_base_metrics <- function(metric){
  if( !( is.character(metric)    & length(metric) ==1 )) stop("metric must be character")
  metrics <- c("TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","TS","ACC","F1", "MCC")
  if (! metric %in% metrics) stop ("Metric not in available metrics")
  invisible(return())
}

assert_performance_metrics <- function(metric){
  if( !( is.character(metric)    & length(metric) ==1 )) stop("metric must be character")
  metrics <- c("accuracy", "f1", "precision", "recall", "auc")
  if (! metric %in% metrics) stop ("Metric not in available metrics")
  invisible(return())
}

assert_parity_metrics <- function(metric){

  if( !( is.character(metric)    & length(metric) ==1 )) stop("metric must be character")
  metrics <- c("TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","TS","ACC","F1", "MCC")
  metrics <- paste0(metrics, "_parity_loss")
  if (! metric %in% metrics) stop ("Metric not in available metrics")

  invisible(return())
}

assert_equal_parameters <- function(x, parameter) {
  param_to_compare <- x[[1]][[parameter]]

  for (obj in x){
     if (obj[[parameter]] != param_to_compare) stop("Parameters have different values")
  }
}



