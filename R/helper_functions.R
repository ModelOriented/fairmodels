### HELPER FUNCTIONS ###


unique_metrics <- function(){
  out <- c("TPR", 'TNR', 'PPV', 'NPV','TS','STP','ACC','F1','MCC')
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
  assert_parity_metrics(metric)
}

assert_performance_metrics <- function(metric){
  if( !( is.character(metric)    & length(metric) ==1 )) stop("metric must be character")
  metrics <- c("accuracy", "f1", "precision", "recall", "auc")
  if (! metric %in% metrics) stop ("Metric not in available metrics")
  invisible(return())
}

assert_parity_metrics <- function(metric){

  if( !( is.character(metric)    & length(metric) ==1 )) stop("metric must be character")
  metrics <- c("TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","TS","STP","ACC","F1", "MCC")
  if (! metric %in% metrics) stop ("Metric not in available metrics")

  invisible(return())
}


assert_equal_parameters <- function(x, parameter) {
  param_to_compare <- x[[1]][[parameter]]

  for (obj in x){
     if (obj[[parameter]] != param_to_compare) stop("Parameters have different values")
  }
}


extract_data  <- function(x, parameter){

  data_list         <- lapply(x, function(x) x[[parameter]])
  data              <- do.call("rbind", data_list)
  return(data)
}

assert_different_label <- function(x){

  labels <- unlist(lapply(x, function(x) x$label))

  if (length(labels) != length(unique(labels))) stop("Some models have the same fairness labels, be sure to print/plot objects with different label ")

}


get_objects <- function(x, class){

  stopifnot(class(x) == "list")

  explainers <- list()
  j <- 1

  for (i in seq_along(x)){
    if (class(x[[i]]) == class) {
      explainers[[j]] <- x[[i]]
      j               <- j + 1
    }
  }

  return(explainers)
}


colors_fairmodels <- function(n = 2){
# bases on drwhy color guide
# 13 distinct colors needed
  if (n < 8){
    return(DALEX::colors_discrete_drwhy(n = n))
  }
  if (n == 8) {
    return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD"))
    }
  if (n == 9){
   return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0"))
  }
  if (n == 10) {
    return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63"))
  }
  if (n == 11){
   return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63",
                     "#733E6B"))
  }
  if (n == 12){

   return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63",
                     "#733E6B",
                     "#9fdf9f"))
  }

  if (n == 13){
    return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63",
                     "#733E6B",
                     "#9fdf9f",
                     "#794469"))
  }

    c(DALEX::colors_discrete_drwhy(n = 7),
      "#B622AD",
      "#c295f0",
      "#037B63",
      "#733E6B",
      "#9fdf9f",
      "#794469")[((0:(n - 1))%%13) + 1]

}



quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


############# checks #############

check_unique_names <- function(x){
  return(length(unique(names(x))) == length(names(x)))
}

check_names_in_names_vector <- function(x, y){
  return(names(x) %in% y)
}


check_list_elements_numeric <- function(x){
  stopifnot(is.list(x))
  return(all(is.numeric(unlist(x))))
}


check_values <- function(x, lower, upper){
  return((all(x >= lower) & all(unlist(x) <= upper)))
}

check_if_numeric_and_single <- function(x){
  return((is.numeric(x) & length(x) == 1))
}





































