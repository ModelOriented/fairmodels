#' Reweight classes
#'
#' @description Function returns weights for model training. The purpose of this weights is to mitigate bias in statistical parity w
#' In fact this could worsen the performance in other fairness metrics. This affects also model's performance metrics (accuracy).
#'
#' @details Method produces weights for each subgroup for each class. Firstly assumes that protected variable and class are independent and
#' calculates expected probability of this certain event (that subgroup == a and class = c). Than it calculates the actual probability of this event based
#' on empirical data. Finally the weight is quotient of those probabilities
#'
#'
#' @references This method was implemented based on Kamiran, Calders 2011 \url{https://link.springer.com/content/pdf/10.1007/s10115-011-0463-8.pdf}
#'
#' @param protected factor, protected variables with subgroups as levels (sensitive attributes)
#' @param y numeric, vector with classes 0 and 1, where 1 means favorable class.
#'
#' @return numeric, vector of weights
#' @export
#'
#'



reweight_classes <- function(protected, y){
  stopifnot(is.factor(protected))
  stopifnot(is.numeric(y))
  stopifnot(length(y) == length(protected))
  if (! (all(unique(y) == c(1,0)) | all(unique(y) == c(0,1)) )) stop("y must be numeric vector with values 0 and 1")

  protected_levels <- levels(protected)
  n                <- length(y)
  weight_vector    <- rep(NA, n)

for (subgroup in protected_levels){
  # c for class
  for (c in unique(y)){
    # number of observations in subgroup
    Xs <- sum(protected == subgroup)
    # number of observations in class
    Xc <- sum(y == c)
    # number of observations in class in subgroup
    Xsc <- sum(protected == subgroup & y == c)
    # formula for constructing the weights
    Wsc <- (Xs * Xc)/(n*Xsc)
    weight_vector[protected == subgroup & y == c] <- Wsc
  }
}
  return(weight_vector)
}











