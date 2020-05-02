#' Group confusion matrices
#'
#' @description Calculates coufusion matrices for each subgroup
#'
#' @param data data frame
#' @param group \code{character} name of column with group
#' @param probs \code{character} name of column with probabilities
#' @param outcome \code{character} name of column with outcome
#' @param outcome_numeric \code{numeric} vector of outcome
#' @param cutoff \code{numeric} cutoff for probabilities, deafult = 0.5
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
#' @rdname group_metrices
#'
#' @examples
#' data("compas")
#'
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' y_prob <- glm_compas$fitted.values
#'
#' data <- compas
#' data$probabilities <- y_prob
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' group_matrices(data,
#'                "Ethnicity",
#'                 outcome = "Two_yr_Recidivism",
#'                  outcome_numeric = y_numeric,
#'                  cutoff = rep(0.45,6))
#'
#'
#'


group_matrices <- function(data, group, probs = "probabilities", outcome, outcome_numeric, cutoff){

  data$outcome_numeric = outcome_numeric

  if(!is.factor(data[,group])) stop("\ndata[,group] is not factor\n")

  group_levels <- levels(data[,group])

  group_confusion_metrices <- list()

  for (i in seq_along(group_levels)){
    subgroup <- group_levels[i]
    sub_data <- data[data[,group] == subgroup,]

    observed <- sub_data$outcome_numeric

    # cutoff is ith element of vector, threshold for group
    cm <- confusion_matrix(sub_data[,probs], observed, cutoff = cutoff[i])

    if (cm$tp == 0 | cm$fp == 0 | cm$tn == 0 | cm$fn == 0) message("\n0's appear in confusion matrix for group: ", subgroup )

    group_confusion_metrices[[i]] <- cm
    names(group_confusion_metrices)[i] <- subgroup
  }
  class(group_confusion_metrices) <- "group_matrices"
  return(group_confusion_metrices)
}

