#' Create Fairness Object
#'
#' @description Create fairness object which measures different fairness metrics and wraps data, explainers and parameters in useful object. This is fundamental object in this package
#' It allows to visualize fairness metrics in many ways and compare models on both fairness and performance level. Enables user to handle many models at once, to choose best by having in mind different
#' aspects and metrics. Some plots are more useful when many models are given and some when 2-3 are passed.
#'
#' @param data Data frame to be used
#' @param x DALEX explainer
#' @param ... possibly more DALEX explainers
#' @param outcome the target of classification
#' @param group protected group/variable that
#' @param base in regard to what subgroup parity loss is calculated
#' @param cutoff threshold for probability, can be vector of thresholds with diferent value for each level of group, deafult 0.5
#'
#' @return An object of class \code{fairness object} which is a list with elements:
#' \itemize{
#' \item metric_data - data.frame containing parity loss for various fairness metrics. Created with following metrics:
#' \itemize{
#'
#' \item TPR - True Positive Rate (Sensitivity, Recall, Equal Odds)
#' \item TNR - True Negative Rate (Specificity)
#' \item PPV - Positive Predictive Value (Precision)
#' \item NPV - Negative Predictive Value
#' \item FNR - False Negative Rate
#' \item FPR - False Positive Rate
#' \item FDR - False Discovery Rate
#' \item FOR - False Omision Rate
#' \item TS - Threat Score
#' \item ACC - Accuracy
#' \item F1 - F1 Score
#' \item MCC - Matthews correlation coefficient
#' }
#'
#' M_parity_loss = sum(abs(metric - base_metric))
#'
#' where:
#'
#' M - some metric mentioned above
#'
#' metric - vector of metrics from each subgroup
#'
#' base_metric - scalar, value of metric for base subgroup
#'
#' \item groups_data - metrics across groups, with base but not summarised for explainer
#'
#' \item explainers  - list of DALEX explainers
#'
#' \item data - data
#'
#' \item ...         - other parameters passed to function
#' }
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
#' fobject <-create_fairness_object(explainer_glm, explainer_rf,
#'                              outcome = "Two_yr_Recidivism",
#'                              group = "Ethnicity",
#'                              base = "Caucasian",
#'                              cutoff = 0.5)
#' plot(fobject)
#'
#' @export
#' @rdname create_fairness_object
#'
#' @references
#' Fairness object took inspiration from R package \code{fairness} by kozodoi \url{https://github.com/kozodoi/fairness}. Some parameters where designed to show resemblance.
#'
#'



create_fairness_object <- function(x,
                                   ...,
                                   data = NULL,
                                   outcome,
                                   group,
                                   base = NULL,
                                   cutoff = NULL) {

# Error handling

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

  # cutoff handling
  num_levels <- length(levels(data[,group]))
  if (is.null(cutoff)) cutoff <- rep(0.5, num_levels)
  if (! is.numeric(cutoff)) stop("cutoff must be numeric scalar/ vector")
  if ( any(cutoff > 1) | any(cutoff < 0)) stop("cutoff must have values between 0 and 1")
  if (length(cutoff) == 1) cutoff <- rep(cutoff, num_levels)

  # cutoff should be now lenght of group's levels
  if (length(cutoff) != num_levels) stop("cutoff must be either lenght 1 or length of group's levels")

# Data extraction

  # explainers from function
  explainers <- c(list(x), list(...))

  # from explainers
  labels <- rep(0, length(explainers))

  for (i in seq_along(explainers)){
    labels[i] <- explainers[[i]]$label
  }

  # explainers must have unique labels
  if (length(unique(labels)) != length(labels) ) stop("Explainers don't have unique labels (use 'label' parameter while creating dalex explainer)")

  n <- length(explainers)
  m <- ncol(data)

  # fairness matrix
  fairness_matrix   <- matrix(nrow = n, ncol = 13) # WARNING if number of metrics changed, change this

  explainers_groups <- list(rep(0,n))

  fairness_labels   <- paste0(c("TPR",
                                "TNR",
                                "PPV",
                                "NPV",
                                "FNR",
                                "FPR",
                                "FDR",
                                "FOR",
                                "TS",
                                "ACC",
                                "F1",
                                "MCC"),
                                "_parity_loss")

  exp_labels <- rep(0,n)

  for (i in seq_along(explainers)) {

    data$probabilities <- explainers[[i]]$y_hat

    #colnames(data)[m + 1] <- "probabilities"
    label <- explainers[[i]]$label

    group_matrices <- group_matrices(data,
                                     group = group,
                                     outcome = outcome,
                                     outcome_numeric = explainers[[i]]$y,
                                     cutoff = cutoff)

    # group metric matrix
    gmm <- calculate_group_fairness_metrics(group_matrices)

    # from every column in matrix subtract base column, then get abs value
    # in other words we measure distance between base group metric's score and other
    # groups metric scores

    gmm_scaled      <- abs(apply(gmm, 2 , function(x) x  - gmm[,base]))
    gmm_loss        <- rowSums(gmm_scaled)
    names(gmm_loss) <- paste0(names(gmm_loss),"_parity_loss")

    fairness_matrix[i, ] <- c(gmm_loss,label)

    # every group value for every metric for every explainer
    metric_list        <- lapply(seq_len(nrow(gmm)), function(j) gmm[j,])
    names(metric_list) <- rownames(gmm)

    explainers_groups[[i]]      <- metric_list
    names(explainers_groups)[i] <- label

    exp_labels[i] <- label
  }

  names(explainers_groups) <- exp_labels

  # as data frame and making numeric

  fairness_df   <- as.data.frame(fairness_matrix)
  n_col         <- ncol(fairness_df)

  fairness_df[, 1:(n_col-1)]   <- apply(fairness_df[, 1:(n_col-1)], 2, as.numeric)

  colnames(fairness_df)        <- fairness_labels
  colnames(fairness_df)[n_col] <- "label"

  # S3 object
  fairness_object <- list(metric_data = fairness_df,
                          groups_data = explainers_groups,
                          explainers  = explainers,
                          data        = data,
                          cutoff      = cutoff,
                          outcome     = outcome,
                          group       = group,
                          base        = base)

  class(fairness_object) <- "fairness_object"

  return(fairness_object)
}



