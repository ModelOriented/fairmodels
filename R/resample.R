#' Resample
#'
#' @description Method of bias mitigation. Similarly to \code{reweight_classes} this method computes desired number of observations if the protected variable is independent
#' from y and on this basis decides if this subgroup with certain class (+ or -) should be more or less numerous. Than performs oversampling or undersampling depending on the case.
#'
#' @references This method was implemented based on Kamiran, Calders 2011 \url{https://link.springer.com/content/pdf/10.1007/s10115-011-0463-8.pdf}
#'
#' @param protected factor, protected variables with subgroups as levels (sensitive attributes)
#' @param y numeric, vector with classes 0 and 1, where 1 means favorable class.
#' @param type character, either (default) 'uniform' or 'preferential'
#' @param probs numeric, vector with probabilities for preferential sampling
#'
#' @return numeric vector of indexes
#' @export


resample <- function(protected, y, type = "uniform",  probs = NULL){

  stopifnot(is.factor(protected))
  stopifnot(is.numeric(y))
  stopifnot(length(y) == length(protected))
  stopifnot(type == 'uniform' | type == 'preferential')
  if (! (all(unique(y) == c(1,0)) | all(unique(y) == c(0,1)) )) stop("y must be numeric vector with values 0 and 1")

  protected_levels <- levels(protected)
  n                <- length(y)
  weight_vector    <- rep(NA, n)

  if (type == "preferential"){
  if ( any(probs < 0) | any(probs > 1)) stop("probs have values outside [0,1]")
  }

  protected_levels <- levels(protected)
  n                <- length(y)
  weight_list      <- list()
  # subgroup - class observations
  num_sc <- list()

  for (subgroup in protected_levels){
    num_c <- rep(NA, 2)
    w_vec <- rep(NA, 2)

    for (c in unique(y)){
       # defining number of observations in groups
       num_c[c + 1] <-  sum(protected == subgroup & y == c)

       # number of observations in subgroup
       Xs <- sum(protected == subgroup)
       # number of observations in class
       Xc <- sum(y == c)
       # number of observations in class in subgroup
       Xsc <- sum(protected == subgroup & y == c)
       # formula for constructing the weights
       w_vec[c+1] <- (Xs * Xc)/(n*Xsc)

    }
    num_sc <- append(num_sc , list(num_c))
    weight_list <- append(weight_list, list(w_vec))
  }
  names(num_sc)      <- protected_levels
  names(weight_list) <- protected_levels

  expected_size      <- ceiling(unlist(num_sc) * unlist(weight_list))
  dim(expected_size) <- c(2,2)
  expected_size      <- t(expected_size)

  expected_size        <- lapply(seq_len(ncol(expected_size)), function(i) expected_size[i,])
  names(expected_size) <- protected_levels

  index_vec <- c()
  if (type == "uniform"){
    for (subgroup in protected_levels){
      for (i in c(1,2)){
        # for expected size bigger than population - with replacement
        if (expected_size[[subgroup]][i] > num_sc[[subgroup]][i]){
          indexes_pool   <- seq_len(length(y))[protected == subgroup & y == (i-1)]
          chosen_indexes <- sample(indexes_pool, expected_size[[subgroup]][i], replace = TRUE)
          index_vec      <- append(index_vec, chosen_indexes)
        } else {
          indexes_pool   <- seq_len(length(y))[protected == subgroup & y == (i-1)]
          chosen_indexes <- sample(indexes_pool, expected_size[[subgroup]][i])
          index_vec <- append(index_vec, chosen_indexes)
          }}}
  }
  if (type == 'preferential'){
    for (subgroup in protected_levels){
      for (i in c(1,2)){
        rest  <-  expected_size[[subgroup]][i] %% num_sc[[subgroup]][i]
        # if expected size is less or equal to pop. size getting only those closer to border
        if (expected_size[[subgroup]][i] <= num_sc[[subgroup]][i]){
         indexes_pool <- c()
        } else {times_to_sample <- floor(expected_size[[subgroup]][i] / num_sc[[subgroup]][i])
          indexes_pool    <- seq_len(length(y))[protected == subgroup & y == (i-1)]
          indexes_pool    <- rep(indexes_pool, times_to_sample)
        }
         if (i == 1){
            # if i == 1 than class == 0 and we get indexes closest to border
            part_of_probs   <- probs[probs < 0.5]
            rest_of_indexes <- order(part_of_probs)[(length(part_of_probs)-rest +1):(length(part_of_probs))]
          } else {
            # else closest to border, but upper side
            part_of_probs   <- probs[probs >= 0.5]
            rest_of_indexes <- order(part_of_probs)[1:rest]
          }
          indexes_pool <- append(indexes_pool, rest_of_indexes)
          index_vec <- append(index_vec, indexes_pool)
        }
    }
  }

  return(index_vec)
}





























