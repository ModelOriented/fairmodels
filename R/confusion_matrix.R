#' Confusion matrix
#' @param predicted predicted from probs
#' @param observed actual values
#' @param true_value true
#'
#' @export
#' @rdname confusion_matrx

confusion_matrix <- function(predicted, observed , true_value){
  stopifnot(sort(unique(predicted)) == sort(unique(observed)))

  confmat <- matrix(0,nrow = 2, ncol = 2 )

  tp <- sum((observed == true_value) & (predicted == true_value))
  fp <- sum((observed != true_value) & (predicted == true_value))
  tn <- sum((observed != true_value) & (predicted != true_value))
  fn <- sum((observed == true_value) & (predicted != true_value))

  confusion_matrix <- list(tp = tp, fp = fp, tn = tn, fn = fn)
  class(confusion_matrix) <- "confusion_matrix"
  return(confusion_matrix)
}
