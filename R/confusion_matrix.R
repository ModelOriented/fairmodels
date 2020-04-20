#' Confusion matrix
#'
#' @param predicted predicted from probs
#' @param observed actual values
#' @param true_value true
#' @param cutoff
#'
#' @export
#' @rdname confusion_matrx

confusion_matrix <- function(probs, observed , cutoff = 0.5){
  stopifnot(length(probs) == length(observed))

  tp = sum((observed == 1)  * (probs >= cutoff))
  fp = sum((observed == 0) * (probs >= cutoff))
  tn = sum((observed == 0)  * (probs < cutoff))
  fn = sum((observed == 1) * (probs < cutoff))

  confusion_matrix <- list(tp = tp, fp = fp, tn = tn, fn = fn)
  class(confusion_matrix) <- "confusion_matrix"
  return(confusion_matrix)
}
