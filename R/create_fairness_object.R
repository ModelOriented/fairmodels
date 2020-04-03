#' Create Fairness Object
#'
#' @param data Data frame to be used
#' @param x DALEX explainer
#' @param ... more dalex explainers
#' @param outcome the target of classification
#' @param group protected group/variable that
#' @param base in regard to what subgroup of group
#'
#' @return An object of class \code{fairness object}
#'
#' It's a list with following fields:
#'
#'

#'
#' @examples
#' library(DALEX)
#' library(ranger)
#'
#' compas <- fairness::compas[,1:7]
#'
#' rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
#' explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)
#'
#'cfo <-create_fairness_object(explainer_glm, explainer_rf,  outcome = "Two_yr_Recidivism", group = "ethnicity", base = "Caucasian"  )
#
#' @export create_fairness_object
#' @rdname create_fairness_object



create_fairness_object <- function(x,
                                   ...,
                                   data = NULL,
                                   outcome,
                                   group,
                                   base) {

  # check if data provided, if not get data from first explainer
  if (is.null(data)) {
    data = x$data
    cat("Getting data from first (", crayon::green(x$label),")  explainer \n")
  }

  # checking if explainers

  # explainers from function
  explainers <- c(list(x), list(...))

  n <- length(explainers)
  m <- ncol(data)

  # fairness matrix
  fairness_matrix <- matrix(nrow = n, ncol = 9)

  # labels for future columns
  fairness_labels <- c("equal_odds", "pred_rate_parity", "acc_parity", "fnr_parity", "fpr_parity", "npv_parity", "spec_parity", "mcc_parity", "model labels")

  for (i in seq_along(explainers)) {
    data$probabilities <- explainers[[i]]$y_hat
    #colnames(data)[m + 1] <- "probabilities"

    eqo <- fairness::equal_odds(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)
    pr  <- fairness::pred_rate_parity(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)
    acc <- fairness::acc_parity(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)
    fnr <- fairness::fnr_parity(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)
    fpr <- fairness::fpr_parity(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)
    npv <- fairness::npv_parity(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)
    spc <- fairness::spec_parity(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)
    # necessary?
    mcc <- fairness::mcc_parity(data = data, outcome = outcome, group = group, probs = "probabilities", base = base)

    metrics <- list(eqo, pr, acc, fnr, fpr, npv, spc, mcc)
    class(metrics) <- "fairness_metrics"
    metrics <- lapply(metrics, absolute)
    metrics <- unlist(metrics)
    metrics <- c(metrics, explainers[[i]]$label)

    fairness_matrix[i, ] <- unlist(metrics)
  }

  # as data frame and making numeric
  fairness_df        <- as.data.frame(fairness_matrix)
  fairness_df[, 1:8] <- apply(fairness_df[, 1:8], 2, as.numeric)
  colnames(fairness_df) <- fairness_labels

  # S3 object
  fairness_object <- list(metric_data = fairness_df, explainers = explainers, data = data)
  class(fairness_object) <- "fairness_object"

  return(fairness_object)
}
