#' Disparate impact remover
#'
#' @param data data.frame, data to be transformed
#' @param protected_colname factor, vector containing sensitive information
#' @param features_to_transform character, vector of column names to be transformed. Columns must have numerical, ordinal values
#' @param lambda numeric, amount of repair desired. Value from 0 to 1, where 0 will return unchanged dataset and 1 fully repaired dataset
#'
#' @return data.frame
#' @export
#'
#'

disparate_impact_remover <- function(data,
                                     protected,
                                     features_to_transform,
                                     lambda){

  # to add - different approach
  # ranks1 <- 1:length(X)/length(X)
  #
  # ranks <- ecdf(X)Fx(X)
  #
  # lambda <- 0.3
  #
  # ranks_hat <- sapply(ranks, function(x) (1-lambda)*x + lambda/2)
  #
  # X_new <- X[floor(ranks_hat*length(X))]

  ###############  error handling  ###############

  if (is.null(data)) stop("You must provide a dataframe")
  if (! all(features_to_transform %in% colnames(data))) stop("features to transform must be array with column names to repair")

  if (! all(apply(data[features_to_transform], 2, function(x) is.numeric(x)))) stop("features to transform must be numeric columns in data")
  if (! is.factor(protected)) stop("protected must be factor with levels acting as different protected subgroups")
  if (length(lambda) != 1) stop ("lambda must be single numeric value")
  if (lambda > 1 | lambda < 0  ) stop ("lambda values must be between 0 and 1")

  data_repaired <- data
  protected_levels <- levels(protected)

  for (feature in features_to_transform){

    # list of Fx^-1 as percentiles
    quantiles <- list()
    num_buckets <- min(sapply(protected_levels, function(x) length(unique(data[protected == x, feature]))))

    if (num_buckets > 101) num_buckets <- 101

    # values of buckets
    bucket_values <- cumsum(rep(100/(num_buckets-1), num_buckets-1))

    for (subgroup in protected_levels){
      Y <- data[protected == subgroup, feature]

      # now let this be product of Fx^-1

      quantiles[[subgroup]] <- quantile(Y, probs = seq(0,1, length.out = num_buckets))

    }

    # list of Fa^-1 - median of inversed distributions
    # which minimizes earth mover distance

    inversed_Fa <- rep(NA, num_buckets)

    for (i in seq_len(num_buckets)){
      inversed_Fa[i] <- median(sapply(quantiles, function(x) x[i]))
    }

    for (subgroup in protected_levels){
      Y <- data[protected == subgroup, feature]

      inversed_Fx <- quantile(Y, probs = seq(0,1, length.out = num_buckets))

      # each position corresponds to percentile
      inversed_Fa_fixed <- (1-lambda)*inversed_Fx + lambda*inversed_Fa

      Y_bucketized <- bucketize(num_buckets, ecdf(Y)(Y))

      levels(Y_bucketized) <- inversed_Fa_fixed
      Y_repaired <- as.numeric(as.character(Y_bucketized))

      data_repaired[protected == subgroup , feature] <- Y_repaired
    }

  }
  return(data_repaired)
}


bucketize <- function(num_buckets, x){
  ## this helper function "bucketizes" the data making boundaries between quantiles (q1, q2, the boundary is (q1+q2)/2 )
  ## the x is binned and assigned to certain buckets


  bin_borders <- 2*(num_buckets-1)
  boundry     <- 100/bin_borders
  y           <- cumsum(rep(boundry, bin_borders))
  bins        <- y[seq(1,2*(num_buckets-1),2)]
  # add 0 and 100
  bins        <- append(append(0, bins), 100)

  # make the cut, each bucket means some quantile
  cut(x*100, bins, include.lowest = TRUE)
}










