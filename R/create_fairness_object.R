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
                                   true_value = 1,
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

  #if (is.null(outcome_base)) outcome_base <- levels(data[,outcome])[1]


  # explainers from function
  explainers <- c(list(x), list(...))

  n <- length(explainers)
  m <- ncol(data)

  # fairness matrix
  fairness_matrix <- matrix(nrow = n, ncol = 13) # WARNING if number of metrics changed, change this

  explainers_groups <- list(rep(0,n))

  fairness_labels <- paste0(c("TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","TS","ACC","F1", "MCC"),"_parity_loss")

  exp_labels <- rep(0,n)

  for (i in seq_along(explainers)) {

    data$probabilities <- explainers[[i]]$y_hat

    if (cutoff > max(data$probabilities)) stop("Cutoff greater than maximal probability")

    #colnames(data)[m + 1] <- "probabilities"
    label <- explainers[[i]]$label

    group_matrices <- group_matrices(data,
                                     group = group,
                                     true_value = true_value,
                                     outcome = outcome,
                                     outcome_numeric = explainers[[i]]$y,
                                     cutoff = cutoff)

    group_metric_matrix <- create_group_metric_matrix(group_matrices)

    # simple scalling and getting loss
    gmm_scaled <- abs(group_metric_matrix/group_metric_matrix[,base] -1)

    gmm_loss <- rowSums(gmm_scaled)
    names(gmm_loss) <- paste0(names(gmm_loss),"_parity_loss")

    fairness_matrix[i, ] <- c(gmm_loss,label)



    # all information can be taken from here,
    # every group value for every metric for every explainer
    gmm_based <- group_metric_matrix/group_metric_matrix[,base]

    metric_list <- lapply(seq_len(nrow(gmm_based)), function(j) gmm_based[j,])
    names(metric_list) <- rownames(gmm_based)

    explainers_groups[[i]] <- metric_list
    names(explainers_groups)[i] <- label


    exp_labels[i] <- label
  }

  names(explainers_groups) <- exp_labels

  # as data frame and making numeric

  fairness_df        <- as.data.frame(fairness_matrix)
  n_col <- ncol(fairness_df)

  fairness_df[, 1:(n_col-1)] <- apply(fairness_df[, 1:(n_col-1)], 2, as.numeric)

  colnames(fairness_df) <- fairness_labels
  colnames(fairness_df)[n_col] <- "label"

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



