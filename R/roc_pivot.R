#' Reject Option based Classification pivot
#'
#' @description Reject Option based Classifier is postprocessing bias mitigation method. Method changes labels of favourable, privileged and close to cutoff observations to unfavourable
#' and the opposite for unprivileged observations (changing unfavourable and close to cutoff observations to favourable, more in details).
#' By this potentially wrongfully labbeled observations are assigned different labels.
#' Note that in y in DALEX explainer 1 should indicate favourable outcome.
#'
#' @param x DALEX explainer
#' @param protected factor, protected variables with subgroups as levels (sensitive attributes)
#' @param privileged factor/character, level in protected denoting privileged subgroup
#' @param cutoff numeric, threshold for all subgroups
#' @param theta numeric, variable specifies maximal euclidean distance to cutoff resulting ing label switch
#'
#' @details Method implemented implemented based on article (Kamiran, Karim, Zhang 2012). In original implementation labels should be switched. Due to specific DALEX methods
#' probabilities (y_hat) are assigned value in equal distance but other side of cutoff. The method changes explainers y_hat values in two cases.
#' 1. When unprivileged subgroup is within (cutoff - theta, cutoff)
#' 2. When privileged subgroup is within (cutoff, cutoff + theta)
#'
#' @references Kamiran, Karim, Zhang 2012 \url{https://ieeexplore.ieee.org/document/6413831/} ROC method
#'
#' @return DALEX explainer (with changed y_hat)
#' @export
#'

roc_pivot <- function(x, protected, privileged,  cutoff = 0.5, theta = 0.1){

  probs <- x$y_hat
  y     <- x$y

  stopifnot(is.factor(protected))
  stopifnot(is.numeric(y))
  stopifnot(length(y) == length(protected) & length(y) == length(probs))
  if (! (all(unique(y) == c(1,0)) | all(unique(y) == c(0,1)) )) stop("y must be numeric vector with values 0 and 1")
  if (cutoff < 0 | cutoff > 1 | length(cutoff) != 1) stop("cutoff must be single numeric value between 0 and 1")
  if (theta < 0 | theta > 1 | length(theta) != 1) stop("cutoff must be single numeric value between 0 and 1")
  if (!is.numeric(probs) | any(probs > 1) | any(probs < 0) ) stop("probs must be numeric vector with values between 0 and 1")

  protected_levels <- levels(protected)
  if(! (is.character(privileged)|is.factor(privileged)) | !(privileged %in% protected_levels) ) stop("privileged must be character/factor denoting privileged subgroup level in protected variable")

  # ROC affecting only probs close (within +/- theta) to cutoff threshold
  is_close <- abs(probs - cutoff) < theta
  is_privileged <- privileged == protected
  is_favourable <- probs > cutoff

  # if affected by above inverse the predictions
  probs[is_close & is_privileged & is_favourable]   <- cutoff - (probs[is_close & is_privileged & is_favourable] - cutoff)
  probs[is_close & !is_privileged & !is_favourable] <- cutoff + (cutoff - probs[is_close & !is_privileged & !is_favourable])
  # if exceeded boundaries
  probs[probs < 0] <- 0
  probs[probs > 1] <- 1

  x$y_hat <- probs
  return(x)
}


















