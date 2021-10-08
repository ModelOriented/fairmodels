#' Disparate impact remover
#'
#' Disparate impact remover is a pre-processing bias mitigation method. It removes bias hidden in numeric columns in data. It changes distribution of ordinal features of data with regard to
#' earth mover distance. It works best if among subgroups there is similar number of observations.
#'
#' @references This method was implemented based on Feldman, Friedler, Moeller, Scheidegger, Venkatasubramanian 2015 \url{https://arxiv.org/pdf/1412.3756.pdf}
#'
#' @details This is implementation of geometric method which preserves ranks unlike combinatorial repair. \code{lambda} close to 1 denotes that distributions will be very close to each other
#' and \code{lambda} close to 0 means that densities will barely change. Note that although \code{lambda} equal 0 should mean that original data will be returned, it usually changes distributions slightly due to
#' pigeonholing. The number of pigeonholes is fixed and equal to min{101, unique(a)}, where a is vector with values for subgroup. So if some subgroup is not numerous and
#' the distribution is discrete with small number of variables then there will be small number of pigeonholes. It will affect data significantly.
#'
#' @param data \code{data.frame}, data to be transformed
#' @param protected factor, vector containing sensitive information such as gender, race etc... If vector is character it will transform it to factor.
#' @param features_to_transform character, vector of column names to be transformed. Columns must have numerical, ordinal values
#' @param lambda numeric, amount of repair desired. Value from 0 to 1, where 0 will return almost unchanged dataset and 1 fully repaired dataset
#'
#'
#'
#' @return repaired data (\code{data.frame} object)
#' @export
#'
#' @examples
#'
#' library("ggplot2")
#'
#' set.seed(1)
#' # custom data frame with kind and score
#' custom_data <- data.frame(
#'   kind = as.factor(c(rep("second", 500), rep("first", 500))),
#'   score = c(rnorm(500, 400, 40), rnorm(500, 600, 100))
#' )
#'
#' ggplot(custom_data, aes(score, fill = kind)) +
#'   geom_density(alpha = 0.5)
#'
#' fixed_data <- disparate_impact_remover(
#'   data = custom_data,
#'   protected = custom_data$kind,
#'   features_to_transform = "score",
#'   lambda = 0.8
#' )
#'
#' ggplot(fixed_data, aes(score, fill = kind)) +
#'   geom_density(alpha = 0.5)
#'
#' # lambda 1 gives identical distribution, lambda 0 (almost) original distributions
#'
#' fixed_data_unchanged <- disparate_impact_remover(
#'   data = custom_data,
#'   protected = custom_data$kind,
#'   features_to_transform = "score",
#'   lambda = 0
#' )
#'
#' ggplot(fixed_data_unchanged, aes(score, fill = kind)) +
#'   geom_density(alpha = 0.5)
#'
#'
#' fixed_data_fully_changed <- disparate_impact_remover(
#'   data = custom_data,
#'   protected = custom_data$kind,
#'   features_to_transform = "score",
#'   lambda = 1
#' )
#'
#' ggplot(fixed_data_fully_changed, aes(score, fill = kind)) +
#'   geom_density(alpha = 0.5) +
#'   facet_wrap(kind ~ ., nrow = 2)
disparate_impact_remover <- function(data,
                                     protected,
                                     features_to_transform,
                                     lambda = 1) {
  if (is.null(data)) stop("You must provide a dataframe")
  if (!all(features_to_transform %in% colnames(data))) stop("features to transform must be array with column names to repair")

  if (!all(apply(data[features_to_transform], 2, function(x) is.numeric(x)))) stop("features to transform must be numeric columns in data")
  if (is.character(protected)) protected <- as.factor(protected)
  if (!is.factor(protected)) stop("protected must be factor with levels acting as different protected subgroups")
  if (!check_if_numeric_and_single(lambda)) stop("lambda must be single numeric value between 0 and 1")

  data_repaired <- data
  protected_levels <- levels(protected)

  for (feature in features_to_transform) {

    # list of Fx^-1 as percentiles
    quantiles <- list()
    num_buckets <- min(sapply(protected_levels, function(x) length(unique(data[protected == x, feature]))))

    if (num_buckets > 101) num_buckets <- 101

    # values of buckets
    bucket_values <- cumsum(rep(100 / (num_buckets - 1), num_buckets - 1))

    for (subgroup in protected_levels) {
      Y <- data[protected == subgroup, feature]

      # now let this be product of Fx^-1

      quantiles[[subgroup]] <- stats::quantile(Y, probs = seq(0, 1, length.out = num_buckets))
    }

    # list of Fa^-1 - median of inversed distributions
    # which minimizes earth mover distance

    inversed_Fa <- rep(NA, num_buckets)

    for (i in seq_len(num_buckets)) {
      inversed_Fa[i] <- stats::median(sapply(quantiles, function(x) x[i]))
    }

    for (subgroup in protected_levels) {
      Y <- data[protected == subgroup, feature]

      inversed_Fx <- stats::quantile(Y, probs = seq(0, 1, length.out = num_buckets))

      # each position corresponds to percentile
      inversed_Fa_fixed <- (1 - lambda) * inversed_Fx + lambda * inversed_Fa

      Y_bucketized <- bucketize(num_buckets, stats::ecdf(Y)(Y))

      levels(Y_bucketized) <- inversed_Fa_fixed
      Y_repaired <- as.numeric(as.character(Y_bucketized))

      data_repaired[protected == subgroup, feature] <- Y_repaired
    }
  }
  return(data_repaired)
}


bucketize <- function(num_buckets, x) {
  ## this helper function "bucketizes" the data making boundaries between quantiles (q1, q2, the boundary is (q1+q2)/2 )
  ## the x is binned and assigned to certain buckets


  bin_borders <- 2 * (num_buckets - 1)
  boundry <- 100 / bin_borders
  y <- cumsum(rep(boundry, bin_borders))
  bins <- y[seq(1, 2 * (num_buckets - 1), 2)]
  # add 0 and 100
  bins <- append(append(0, bins), 100)

  # make the cut, each bucket means some quantile
  cut(x * 100, bins, include.lowest = TRUE)
}
