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


result <- list()

for (subgroup in protected_levels){
  sub_data <- data[data$sex == subgroup,]
  X <- sub_data$score

  # now let this be product of Fx^-1
  result[[subgroup]] <- quantile(X, probs = seq(0,1, length.out = 101))

}

# we have distribution products
percentiles <- rep(NA, 101)

for (i in 1:101){
  percentiles[i] <- median(sapply(result, function(x) x[i]))
}

inversed_Fx <- quantile(X, probs =seq(0,1, length.out = 101))
# each position corresponds to percentile
inversed_Fa <- percentiles


inversed_Fa_fixed <- (1-lambda)*inversed_Fx + lambda*inversed_Fa

Y_repaired <- inversed_Fa_fixed[as.integer(round(ecdf(X)(X),2) * 100) +1L]

# ToDo loop over all columns.
}
