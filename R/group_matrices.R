#' Group confusion matrices
#'
#' Calculates confusion matrices for each subgroup
#'
#' @param probs \code{character} name of column with probabilities
#' @param cutoff \code{numeric} cutoff for probabilities, default = 0.5
#' @param protected vector containing protected variable
#' @param preds numeric, vector with predictions
#'
#' @return \code{group_matrices} object
#' It is a list with values:
#' \itemize{
#' For each subgroup:
#' \item subgroup
#' \itemize{
#' \item{tp}{ - number of true positives}
#' \item{fp}{ - number of false positives}
#' \item{tn}{ - number of true negatives}
#' \item{fn}{ - number of false negatives}
#' }}
#' @export
#' @rdname group_matrices
#'
#' @examples
#' data("compas")
#'
#' glm_compas <- glm(Two_yr_Recidivism ~ ., data = compas, family = binomial(link = "logit"))
#' y_prob <- glm_compas$fitted.values
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism) - 1
#'
#' gm <- group_matrices(compas$Ethnicity,
#'   y_prob,
#'   y_numeric,
#'   cutoff = list(
#'     Asian = 0.45,
#'     African_American = 0.5,
#'     Other = 0.5,
#'     Hispanic = 0.5,
#'     Caucasian = 0.4,
#'     Native_American = 0.5
#'   )
#' )
#'
#' gm
group_matrices <- function(protected, probs, preds, cutoff) {
  protected_levels <- levels(protected)
  group_confusion_matrices <- list()

  group_data <- data.frame(
    preds = preds,
    probs = probs,
    protected = protected
  )

  for (i in seq_along(protected_levels)) {
    subgroup <- protected_levels[i]
    sub_data <- group_data[group_data[, "protected"] == subgroup, ]

    observed <- sub_data$preds
    probabilities <- sub_data$probs

    cm <- confusion_matrix(probabilities, observed, cutoff = cutoff[[subgroup]])

    group_confusion_matrices[[i]] <- cm
    names(group_confusion_matrices)[i] <- subgroup
  }
  class(group_confusion_matrices) <- "group_matrices"
  return(group_confusion_matrices)
}
