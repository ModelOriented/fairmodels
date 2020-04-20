#' Confusion matrix
#'
#' @param predicted predicted from probs
#' @param observed actual values
#' @param true_value true
#' @param cutoff
#'
#' @export
#' @rdname confusion_matrx

confusion_matrix <- function(predicted, observed , cutoff = 0.5, true_value){
  stopifnot(sort(unique(predicted)) == sort(unique(observed)))

  confmat <- matrix(0,nrow = 2, ncol = 2 )

  tp = sum((observed == true_value) * (predicted >= cutoff))
  fp = sum((observed != true_value) * (predicted >= cutoff))
  tn = sum((observed != true_value) * (predicted < cutoff))
  fn = sum((observed == true_value) * (predicted < cutoff))

  confusion_matrix <- list(tp = tp, fp = fp, tn = tn, fn = fn)
  class(confusion_matrix) <- "confusion_matrix"
  return(confusion_matrix)
}
