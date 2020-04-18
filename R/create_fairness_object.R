#' Create Fairness Object
#'
#' @param data Data frame to be used
#' @param x DALEX explainer
#' @param ... more dalex explainers
#' @param outcome the target of classification
#' @param group protected group/variable that
#' @param base in regard to what subgroup of group
#' @param outcome_base outcome factor level to be considered as class \code{true}
#' @param cutoff threshold for probability, deafult 0.5
#'
#' @return An object of class \code{fairness object}
#'
#' It's a list with following fields:
#'
#'
#' @examples
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
#' explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)
#'
#' cfo <-create_fairness_object(explainer_glm, explainer_rf,
#'                              outcome = "Two_yr_Recidivism",
#'                              group = "Ethnicity",
#'                              base = "Caucasian",
#'                              cutoff = 0.5)
#'
#'
#' @export
#' @rdname create_fairness_object



create_fairness_object <- function(x,
                                   ...,
                                   data = NULL,
                                   outcome,
                                   group,
                                   base = NULL,
                                   outcome_base = NULL,
                                   cutoff = 0.5) {

  # check if data provided, if not get data from first explainer
  if (is.null(data)) {
    data = x$data
    cat("Getting data from first (", crayon::green(x$label),")  explainer \n")
  }

  # if columns not in data
  if (! outcome %in% colnames(data)) stop(cat(outcome, "is not column name in data \n"))
  if (! group %in% colnames(data))   stop(cat(group, "is not column name in data \n"))

  # if base = null take first from data
  if (is.null(base)) base <- data[1, group]

  if (!is.factor(data[,outcome])){
    cat("\nChanging outcome to factor\n")
    data[,outcome] <- as.factor(data[,outcome])
    }

  if (is.null(outcome_base)) outcome_base <- levels(data[,outcome])[1]

  # explainers from function
  explainers <- c(list(x), list(...))

  n <- length(explainers)
  m <- ncol(data)

  # fairness matrix
  fairness_matrix <- matrix(nrow = n, ncol = 9) # WARNING if number of metrics changed, change this

  explainers_groups <- list(rep(0,n))


  # labels for future columns
  fairness_labels <- c("equal_odds",
                       "pred_rate_parity",
                       "acc_parity",
                       "fnr_parity",
                       "fpr_parity",
                       "npv_parity",
                       "spec_parity",
                       "mcc_parity",
                       "model labels")

  for (i in seq_along(explainers)) {

    data$probabilities <- explainers[[i]]$y_hat
    #colnames(data)[m + 1] <- "probabilities"
    label <- explainers[[i]]$label


    eqo <- fairness::equal_odds(data = data,
                                outcome = outcome,
                                group = group,
                                probs = "probabilities",
                                base = base,
                                cutoff = cutoff,
                                outcome_base = outcome_base)
    pr  <- fairness::pred_rate_parity(data = data,
                                      outcome = outcome,
                                      group = group,
                                      probs = "probabilities",
                                      base = base,
                                      cutoff = cutoff,
                                      outcome_base = outcome_base)
    acc <- fairness::acc_parity(data = data,
                                outcome = outcome,
                                group = group,
                                probs = "probabilities",
                                base = base,
                                cutoff = cutoff,
                                outcome_base = outcome_base)
    fnr <- fairness::fnr_parity(data = data,
                                outcome = outcome,
                                group = group,
                                probs = "probabilities",
                                base = base,
                                cutoff = cutoff,
                                outcome_base = outcome_base)
    fpr <- fairness::fpr_parity(data = data,
                                outcome = outcome,
                                group = group,
                                probs = "probabilities",
                                base = base,
                                cutoff = cutoff,
                                outcome_base = outcome_base)
    npv <- fairness::npv_parity(data = data,
                                outcome = outcome,
                                group = group,
                                probs = "probabilities",
                                base = base,
                                cutoff = cutoff,
                                outcome_base = outcome_base)
    spc <- fairness::spec_parity(data = data,
                                 outcome = outcome,
                                 group = group,
                                 probs = "probabilities",
                                 base = base,
                                 cutoff = cutoff,
                                 outcome_base = outcome_base)
    # necessary?
    mcc <- fairness::mcc_parity(data = data,
                                outcome = outcome,
                                group = group,
                                probs = "probabilities",
                                base = base,
                                cutoff = cutoff,
                                outcome_base = outcome_base)

    # cummuleted metrics "how much to base level" over all groups, then summed
    cummulated_metrics <- lapply(list(eqo, pr, acc, fnr, fpr, npv, spc, mcc), absolute)
    cummulated_metrics <- unlist(cummulated_metrics)

    # checking if NaN's were created, changing them to NA
    if(NaN %in% cummulated_metrics){
      nans <- is.nan(cummulated_metrics)
      cummulated_metrics[nans] <- NA

      cat(crayon::red("\nWARNING! NA's created, check fairness_object$groups_data \n\n"))
    }


    # metrics over all groups
    metrics <- list(equal_odds = eqo$Metric[2,],
                    pred_rate_parity =  pr$Metric[2,],
                    acc_parity = acc$Metric[2,],
                    fnr_parity = fnr$Metric[2,],
                    fpr_parity = fpr$Metric[2,],
                    npv_parity = npv$Metric[2,],
                    spec_parity = spc$Metric[2,],
                    mcc_parity = mcc$Metric[2,],
                    label = label)

    # with label
    metrics <- c(metrics, label)

    fairness_matrix[i, ] <- c(cummulated_metrics,label)

    # all information can be taken from here,
    # every group value for every metric for every explainer
    explainers_groups[[i]] <- metrics
  }

  # as data frame and making numeric
  fairness_df        <- as.data.frame(fairness_matrix)
  fairness_df[, 1:8] <- apply(fairness_df[, 1:8], 2, as.numeric)
  colnames(fairness_df) <- fairness_labels

  # S3 object
  fairness_object <- list(metric_data = fairness_df,
                          groups_data = explainers_groups,
                          explainers = explainers,
                          data = data,
                          cutoff = cutoff,
                          outcome = outcome,
                          group = group,
                          base = base)
  class(fairness_object) <- "fairness_object"

  return(fairness_object)
}



absolute <- function(x){
  return(sum(abs(1- x$Metric[2,])))
}
