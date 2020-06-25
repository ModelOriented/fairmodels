#' Group confusion matrices
#'
#' @description Calculates confusion matrices for each subgroup
#'
#' @param data data frame
#' @param group \code{character} name of column with group
#' @param probs \code{character} name of column with probabilities
#' @param outcome \code{character} name of column with outcome
#' @param outcome_numeric \code{numeric} vector of outcome
#' @param cutoff \code{numeric} cutoff for probabilities, default = 0.5
#'
#' @return list with values:
#' \itemize{
#' \item subgroup
#' \itemize{
#' \item tp - true positive values
#' \item fp - false positive values
#' \item tn - true negative values
#' \item fn - false negative values
#' }}
#' @export
#' @rdname group_matrices
#'
#' @examples
#' data("compas")
#'
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' y_prob <- glm_compas$fitted.values
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' gm <- group_matrices(as.numeric(compas$Ethnicity),
#'                      y_prob,
#'                      y_numeric,
#'                      cutoff = rep(0.45,6))
#'
#' gm
#'


group_matrices <- function(protected, probs, preds , cutoff){

  group_values <- unique(as.character(protected))
  group_confusion_metrices <- list()

  group_data <- data.frame(preds = preds,
                           probs = probs,
                           protected = protected  )

  for (i in seq_along(group_values)){
    subgroup <- group_values[i]
    sub_data <- group_data[group_data[,"protected"] == subgroup,]

    observed      <- sub_data$preds
    probabilities <- sub_data$probs

    # cutoff is ith element of vector, threshold for group
    cm <- confusion_matrix(probabilities, observed, cutoff = cutoff[i])

    if (cm$tp == 0 | cm$fp == 0 | cm$tn == 0 | cm$fn == 0) message("\n0's appear in confusion matrix for group: ", subgroup )

    group_confusion_metrices[[i]] <- cm
    names(group_confusion_metrices)[i] <- subgroup
  }
  class(group_confusion_metrices) <- "group_matrices"
  return(group_confusion_metrices)
}

