#' Create Fairness Object
#'
#' @description Fairness check creates fairness object which measures different fairness metrics and wraps data, explainers and parameters in useful object. This is fundamental object in this package
#' It allows to visualize fairness metrics in many ways and compare models on both fairness and performance level. Fairness check
#'
#' @param data Data frame to be used
#' @param x DALEX explainer
#' @param ... possibly more DALEX explainers
#' @param outcome character, the target of classification, column name in data
#' @param group character, protected group/variable with subgroups visible as levels, column name in data
#' @param base character, subgroup, one of levels of group. In regard to what subgroup parity loss is calculated.
#' @param cutoff numeric, threshold for probability, can be vector of thresholds with different value for each level of group (subgroup), default 0.5
#' @param label character, labels for models in fairness object, if \code{NULL} labels from explainers will be extracted
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
#' \item FOR - False Omission Rate
#' \item TS  - Threat Score
#' \item STP - Statistical Parity
#' \item ACC - Accuracy
#' \item F1  - F1 Score
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
#' \item groups_data - metrics across groups, with base but not summarized for explainer
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
                                   cutoff = NULL,
                                   label = NULL) {

# Error handling

  # check if data provided, if not get data from first explainer
  if (is.null(data)) {
    data = x$data
    cat("Getting data from first (", color_codes$green_start,  x$label, color_codes$green_end, ")  explainer \n")
  }

  # if columns not in data
  if (! outcome %in% colnames(data)) stop(cat(outcome, "is not column name in data \n"))
  if (! group %in% colnames(data))   stop(cat(group, "is not column name in data \n"))

  # if base = null take first from data
  if (is.null(base)) base <- as.character(data[1, group])

  # outcome must be factor
  if (!is.factor(data[,outcome])){
    cat("\nChanging outcome to factor\n")
    data[,outcome] <- as.factor(data[,outcome])
  }

  # cutoff handling- if cutoff is null than 0.5 for all subgroups
  group_levels <- length(levels(data[,group]))
  if (is.null(cutoff))                    cutoff <- rep(0.5, group_levels)
  if (! is.numeric(cutoff))               stop("cutoff must be numeric scalar/ vector")
  if ( any(cutoff > 1) | any(cutoff < 0)) stop("cutoff must have values between 0 and 1")
  if (length(cutoff) == 1)                cutoff <- rep(cutoff, group_levels)

  # cutoff should be now length of group's levels
  if (length(cutoff) != group_levels) stop("cutoff must be either lenght 1 or length of group's levels")


  # Data extraction
  explainers <- c(list(x), list(...))

  if (is.null(label)){
    label     <- sapply(explainers, function(x) x$label)
  } else {
    if (length(label) != length(explainers)) stop("Number of labels must be equal to number of explainers")
  }

  # explainers must have unique labels
  if (length(unique(label)) != length(label) ) stop("Explainers don't have unique labels (use 'label' parameter while creating dalex explainer)")

  n_exp <- length(explainers)

  # fairness matrix
  # WARNING if number of metrics changed, change ncol
  # number of metrics must be fixed. If changed add metric to metric labels
  metric_data   <- matrix(nrow = n_exp, ncol = 12)

  explainers_groups <- list(rep(0,n_exp))

  for (i in seq_along(explainers)) {

    data$`_probabilities_` <- explainers[[i]]$y_hat

    group_matrices <- group_matrices(data,
                                     group = group,
                                     outcome = outcome,
                                     outcome_numeric = explainers[[i]]$y,
                                     cutoff = cutoff)

    # group metric matrix
    gmm <- calculate_group_fairness_metrics(group_matrices)

    # from every column in matrix subtract base column, then get abs value
    # in other words we measure distance between base group metrics score and other
    # groups metric scores

    gmm_scaled      <- abs(apply(gmm, 2 , function(x) x  - gmm[,base]))
    gmm_loss        <- rowSums(gmm_scaled)
    names(gmm_loss) <- paste0(names(gmm_loss),"_parity_loss")

    metric_data[i, ] <- gmm_loss

    # every group value for every metric for every explainer
    metric_list                 <- lapply(seq_len(nrow(gmm)), function(j) gmm[j,])
    names(metric_list)          <- rownames(gmm)
    explainers_groups[[i]]      <- metric_list
    names(explainers_groups)[i] <- label[i]

  }

  # as data frame and making numeric
  metric_data   <- as.data.frame(metric_data)

  metric_labels   <- paste0(c("TPR",
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

  colnames(metric_data) <- metric_labels

  # S3 object
  fairness_object <- list(metric_data     = metric_data,
                          groups_data     = explainers_groups,
                          explainers      = explainers,
                          label           = label,
                          data            = data,
                          cutoff          = cutoff,
                          outcome         = outcome,
                          group           = group,
                          base            = base)

  class(fairness_object) <- "fairness_object"
  return(fairness_object)
}


color_codes <- list(yellow_start = "\033[33m", yellow_end = "\033[39m",
                    red_start = "\033[31m", red_end = "\033[39m",
                    green_start = "\033[32m", green_end = "\033[39m")
