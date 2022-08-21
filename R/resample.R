#' Resample
#'
#' Method of bias mitigation. Similarly to \code{reweight} this method computes desired number of observations if the protected variable is independent
#' from y and on this basis decides if this subgroup with certain class (+ or -) should be more or less numerous. Than performs oversampling or undersampling depending on the case.
#' If type of sampling is set to 'preferential' and probs are provided than instead of uniform sampling preferential sampling will be performed. Preferential sampling depending on the case
#' will sample observations close to border or far from border.
#'
#' @references This method was implemented based on Kamiran, Calders 2011 \url{https://link.springer.com/content/pdf/10.1007/s10115-011-0463-8.pdf}
#'
#' @param protected factor, protected variables with subgroups as levels (sensitive attributes)
#' @param y numeric, vector with classes 0 and 1, where 1 means favorable class.
#' @param type character, either (default) 'uniform' or 'preferential'
#' @param probs numeric, vector with probabilities for preferential sampling
#' @param cutoff numeric, threshold for probabilities
#'
#' @return numeric vector of indexes
#' @export
#'
#' @examples
#' data("german")
#'
#' data <- german
#'
#' data$Age <- as.factor(ifelse(data$Age <= 25, "young", "old"))
#' y_numeric <- as.numeric(data$Risk) - 1
#'
#' rf <- ranger::ranger(Risk ~ .,
#'   data = data,
#'   probability = TRUE,
#'   num.trees = 50,
#'   num.threads = 1,
#'   seed = 123
#' )
#'
#' u_indexes <- resample(data$Age, y = y_numeric)
#'
#' rf_u <- ranger::ranger(Risk ~ .,
#'   data = data[u_indexes, ],
#'   probability = TRUE,
#'   num.trees = 50,
#'   num.threads = 1,
#'   seed = 123
#' )
#'
#' explainer_rf <- DALEX::explain(rf,
#'   data = data[, -1],
#'   y = y_numeric,
#'   label = "not_sampled"
#' )
#'
#' explainer_rf_u <- DALEX::explain(rf_u, data = data[, -1], y = y_numeric, label = "sampled_uniform")
#'
#' fobject <- fairness_check(explainer_rf, explainer_rf_u,
#'   protected = data$Age,
#'   privileged = "old"
#' )
#'
#' fobject
#' plot(fobject)
#' \donttest{
#' p_indexes <- resample(data$Age, y = y_numeric, type = "preferential", probs = explainer_rf$y_hat)
#' rf_p <- ranger::ranger(Risk ~ .,
#'   data = data[p_indexes, ],
#'   probability = TRUE,
#'   num.trees = 50,
#'   num.threads = 1,
#'   seed = 123
#' )
#'
#' explainer_rf_p <- DALEX::explain(rf_p,
#'   data = data[, -1], y = y_numeric,
#'   label = "sampled_preferential"
#' )
#'
#' fobject <- fairness_check(explainer_rf, explainer_rf_u, explainer_rf_p,
#'   protected = data$Age,
#'   privileged = "old"
#' )
#'
#' fobject
#' plot(fobject)
#' }
#'
resample <- function(protected, y, type = "uniform", probs = NULL, cutoff = 0.5) {
  if (is.character(protected)) {
    cat("\nchanging protected to factor \n")
    protected <- as.factor(protected)
  }
  stopifnot(is.factor(protected))
  stopifnot(is.numeric(y))
  stopifnot(length(y) == length(protected))
  stopifnot(type == "uniform" | type == "preferential")
  if (!(all(unique(y) == c(1, 0)) | all(unique(y) == c(0, 1)))) stop("y must be numeric vector with values 0 and 1")
  if (!check_if_numeric_and_single(cutoff)) stop("cutoff must be single numeric value")
  if (!check_values(cutoff, 0, 1)) stop("cutoff must be between 0 and 1")

  protected_levels <- levels(protected)
  n <- length(y)
  weight_vector <- rep(NA, n)

  if (type == "preferential") {
    if (is.null(probs)) stop("probs were not provided")
    if (length(probs) != length(y)) stop("probs and y have different lengths")
    if (!check_values(probs, 0, 1)) stop("probs have values outside [0,1]")
  }

  protected_levels <- levels(protected)
  n <- length(y)
  weight_list <- list()

  # subgroup - class observations
  num_sc <- list()

  for (subgroup in protected_levels) {
    num_c <- rep(NA, 2)
    w_vec <- rep(NA, 2)

    for (actual_class in c(0, 1)) {
      # defining number of observations in groups

      num_c[actual_class + 1] <- sum(protected == subgroup & y == actual_class)
      # number of observations in subgroup
      Xs <- sum(protected == subgroup)
      # number of observations in class
      Xc <- sum(y == actual_class)
      # number of observations in class in subgroup
      Xsc <- sum(protected == subgroup & y == actual_class)
      # formula for constructing the weights
      w_vec[actual_class + 1] <- (Xs * Xc) / (n * Xsc)
      # fix for https://github.com/ModelOriented/fairmodels/issues/48
      if (is.infinite(w_vec[actual_class + 1])) {
        w_vec[actual_class + 1] = 0
        warning("Infinite weights, set to 0. Probably sizes of some protected groups are too small")
      }
    }
    num_sc <- append(num_sc, list(num_c))
    weight_list <- append(weight_list, list(w_vec))
  }
  names(num_sc) <- protected_levels
  names(weight_list) <- protected_levels

  expected_size <- matrix(round(unlist(num_sc) * unlist(weight_list)), length(protected_levels), 2, byrow = TRUE)

  expected_size <- lapply(seq_len(nrow(expected_size)), function(i) expected_size[i, ])
  names(expected_size) <- protected_levels

  index_vec <- c()
  if (type == "uniform") {
    for (subgroup in protected_levels) {
      for (i in c(1, 2)) {
        # expected size
        expected <- expected_size[[subgroup]][i]
        # actual size
        actual <- num_sc[[subgroup]][i]
        # logical, what is current case
        current_case <- protected == subgroup & y == (i - 1)

        # for expected size bigger than population - with replacement
        if (expected > actual) {
          indexes_pool <- seq_len(length(y))[current_case]
          if (length(indexes_pool) == 0) next
          if (length(indexes_pool) == 1) {
            chosen_indexes <- rep(indexes_pool, expected)
          } else {
            chosen_indexes <- sample(indexes_pool, size = expected, replace = TRUE)
          }
          index_vec <- append(index_vec, chosen_indexes)
        } else {
          indexes_pool <- seq_len(length(y))[current_case]
          chosen_indexes <- sample(indexes_pool, expected)
          index_vec <- append(index_vec, chosen_indexes)
        }
      }
    }
  }
  if (type == "preferential") {
    for (subgroup in protected_levels) {
      for (i in c(1, 2)) {
        if (length(num_sc[[subgroup]][i]) == 0) next
        indexes_pool <- c()
        # expected size
        expected <- expected_size[[subgroup]][i]
        # actual size
        actual <- num_sc[[subgroup]][i]
        rest <- expected %% actual

        # logical, what is current case
        current_case <- protected == subgroup & y == (i - 1)

        # if expected size is bigger then actual
        if (expected >= actual) {
          times_to_sample <- floor(expected / actual)
          indexes_pool <- seq_len(length(y))[current_case]
          indexes_pool <- rep(indexes_pool, times_to_sample)
        }
        if (i == 1) {
          # if i == 1 than class == 0
          if (rest == 0) {
            rest_of_indexes <- integer(0)
          } else {
            part_of_probs <- probs[current_case]

            if (expected >= actual) {
              # taking highest ranked
              part_of_rest_of_indexes <- order(part_of_probs)[(length(part_of_probs) - rest + 1):(length(part_of_probs))]
            } else {
              # taking lowest ranked
              part_of_rest_of_indexes <- order(part_of_probs)[1:rest]
            }

            rest_of_indexes <- seq_len(length(y))[current_case][part_of_rest_of_indexes]
          }
        } else {
          if (rest == 0) {
            rest_of_indexes <- integer(0)
          } else {
            # else closest to border, but upper side
            part_of_probs <- probs[current_case]

            if (expected >= actual) {
              part_of_rest_of_indexes <- order(part_of_probs)[1:rest]
            } else {
              part_of_rest_of_indexes <- order(part_of_probs)[(length(part_of_probs) - rest + 1):(length(part_of_probs))]
            }
            rest_of_indexes <- seq_len(length(y))[current_case][part_of_rest_of_indexes]
          }
        }
        indexes_pool <- append(indexes_pool, rest_of_indexes)
        index_vec <- append(index_vec, indexes_pool)
      }
    }
  }

  return(index_vec)
}
