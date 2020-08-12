#' Reject Option based Classification pivot
#'
#' Reject Option based Classifier is post-processing bias mitigation method. Method changes labels of favorable, privileged and close to cutoff observations to unfavorable
#' and the opposite for unprivileged observations (changing unfavorable and close to cutoff observations to favorable, more in details).
#' By this potentially wrongfully labeled observations are assigned different labels.
#' Note that in y in DALEX explainer 1 should indicate favorable outcome.
#'
#' @param explainer created with \code{\link[DALEX]{explain}}
#' @param protected factor, protected variables with subgroups as levels (sensitive attributes)
#' @param privileged factor/character, level in protected denoting privileged subgroup
#' @param cutoff numeric, threshold for all subgroups
#' @param theta numeric, variable specifies maximal euclidean distance to cutoff resulting ing label switch
#'
#' @details Method implemented implemented based on article (Kamiran, Karim, Zhang 2012). In original implementation labels should be switched. Due to specific DALEX methods
#' probabilities (y_hat) are assigned value in equal distance but other side of cutoff. The method changes explainers y_hat values in two cases.
#' \cr
#' 1. When unprivileged subgroup is within (cutoff - theta, cutoff)\cr
#' 2. When privileged subgroup is within (cutoff, cutoff + theta)\cr
#'
#' @references Kamiran, Karim, Zhang 2012 \url{https://ieeexplore.ieee.org/document/6413831/} ROC method
#'
#' @return DALEX \code{explainer} with changed y_hat
#' @export
#'
#' @examples
#'
#' data("german")
#' data <- german
#' data$Age <- as.factor(ifelse(data$Age <= 25, "young", "old"))
#' y_numeric <- as.numeric(data$Risk) -1
#'
#' lr_model     <- stats::glm(Risk ~., data = data, family = binomial())
#' lr_explainer <- DALEX::explain(lr_model, data = data[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(lr_explainer,
#'                           protected = data$Age,
#'                           privileged = "old")
#' plot(fobject)
#'
#' lr_explainer_fixed <- roc_pivot(lr_explainer,
#'                                 protected = data$Age,
#'                                 privileged = "old")
#'
#' fobject2 <- fairness_check(lr_explainer_fixed, fobject,
#'                            protected = data$Age,
#'                            privileged = "old",
#'                            label = "lr_fixed")
#' fobject2
#' plot(fobject2)

roc_pivot <- function(explainer, protected, privileged,  cutoff = 0.5, theta = 0.1){

  stopifnot(class(explainer) == "explainer")
  stopifnot(explainer$model_info$type == "classification")

  x <- explainer
  probs <- x$y_hat
  y     <- x$y

  if (is.character(protected)){
    cat("changing protected to factor \n")
    protected <- as.factor(protected)
  }
  stopifnot(is.factor(protected))
  stopifnot(is.numeric(y))
  stopifnot(length(y) == length(protected) & length(y) == length(probs))
  if (! (all(unique(y) == c(1,0)) | all(unique(y) == c(0,1)) )) stop("y must be numeric vector with values 0 and 1")
  if (! check_if_numeric_and_single(cutoff)) stop("cutoff must be single numeric value")
  if (! check_values(cutoff, 0, 1))          stop("cutoff must be between 0 and 1")
  if (! check_if_numeric_and_single(theta))  stop("theta must be single numeric value")
  if (! check_values(theta, 0, 1))           stop("theta must be between 0 and 1")

  protected_levels <- levels(protected)
  if(! (is.character(privileged) | is.factor(privileged)) | !(privileged %in% protected_levels) ) stop("privileged must be character/factor denoting privileged subgroup level in protected variable")

  # ROC affecting only probs close (within +/- theta) to cutoff threshold
  is_close      <- abs(probs - cutoff) < theta
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


















