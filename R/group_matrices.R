#' Group confusion matrices
#'
#' @description Calculates confusion matrices for each subgroup
#'
#' @param probs \code{character} name of column with probabilities
#' @param cutoff \code{numeric} cutoff for probabilities, default = 0.5
#' @param protected vector containing protected variable
#' @param preds numeric, vector with predictions
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

  protected_levels <- levels(protected)
  group_confusion_metrices <- list()

  group_data <- data.frame(preds     = preds,
                           probs     = probs,
                           protected = protected  )

  for (i in seq_along(protected_levels)){
    subgroup <- protected_levels[i]
    sub_data <- group_data[group_data[,"protected"] == subgroup,]

    observed      <- sub_data$preds
    probabilities <- sub_data$probs

    # cutoff is ith element of vector, threshold for group
    cm <- confusion_matrix(probabilities, observed, cutoff = cutoff[i])

    group_confusion_metrices[[i]] <- cm
    names(group_confusion_metrices)[i] <- subgroup
  }
  class(group_confusion_metrices) <- "group_matrices"
  return(group_confusion_metrices)
}

