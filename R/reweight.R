#' Reweight
#'
#' Function returns weights for model training. The purpose of this weights is to mitigate bias in statistical parity.
#' In fact this could potentially worsen the overall performance in other fairness metrics. This affects also model's performance metrics (accuracy).
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
#' @examples
#' data("german")
#'
#' data <- german
#' data$Age <- as.factor(ifelse(data$Age <= 25, "young", "old"))
#' data$Risk <- as.numeric(data$Risk) -1
#'
#' # training 2 models
#' weights <- reweight(protected = data$Age, y = data$Risk)
#'
#' gbm_model          <- gbm::gbm(Risk ~. , data = data)
#' gbm_model_weighted <- gbm::gbm(Risk ~. , data = data, weights = weights)
#'
#' gbm_explainer          <- DALEX::explain(gbm_model, data = data[,-1], y = data$Risk)
#' gbm_weighted_explainer <- DALEX::explain(gbm_model_weighted, data = data[,-1], y = data$Risk)
#'
#' fobject <- fairness_check(gbm_explainer, gbm_weighted_explainer,
#'                           protected = data$Age,
#'                           privileged = "old",
#'                           label = c("original","weighted"))
#' # fairness check
#' fobject
#' plot(fobject)
#'
#' # radar
#' plot(fairness_radar(fobject))


reweight <- function(protected, y){

  if (! is.factor(protected)) {
    cat("\nchanging protected to factor \n")
    protected <- as.factor(protected)
  }


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











