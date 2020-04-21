#' Confusion matrix
#'
#' @param probs probabilities given by model
#' @param observed actual values from outcome
#' @param cutoff cutoff
#'
#' @export
#' @rdname confusion_matrx

confusion_matrix <- function(probs, observed , cutoff = 0.5){
  stopifnot(length(probs) == length(observed))
  stopifnot(is.numeric(probs) & is.numeric(observed))
  stopifnot(is.numeric(cutoff))

  tp = sum((observed == 1)  * (probs >= cutoff))
  fp = sum((observed == 0) * (probs >= cutoff))
  tn = sum((observed == 0)  * (probs < cutoff))
  fn = sum((observed == 1) * (probs < cutoff))

  confusion_matrix <- list(tp = tp, fp = fp, tn = tn, fn = fn)
  class(confusion_matrix) <- "confusion_matrix"
  return(confusion_matrix)
}
