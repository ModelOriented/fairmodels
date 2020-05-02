#' Confusion matrix
#'
#' @description Calculates confusion matrix for given cutoff
#'
#' @param probs probabilities given by model
#' @param observed actual values from outcome
#' @param cutoff cutoff
#'
#' @return list that creates confusion matrix, of class \code{confussion_matrix}
#'
#' @export
#' @rdname confusion_matrx
#'
#'
#' @examples
#'
#' probs    <- rnorm(20, 0.4,0.1)
#' observed <- round(runif(20))
#'
#' confusion_matrix(probs, observed, 0.5)
#'

confusion_matrix <- function(probs, observed , cutoff){
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
