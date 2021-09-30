#' Fairness PCA
#'
#' Calculate PC for metric_matrix to see similarities between models and metrics. If \code{omit_models_with_NA} is set to \code{TRUE} models with NA will be omitted as opposed
#' to default behavior, when metrics are omitted.
#'
#' @param x object of class \code{fairness object}
#' @param omit_models_with_NA logical, if \code{TRUE} omits rows in \code{metric_matrix}, else omits columns (default)
#'
#' @return \code{fairness_pca} object
#' It is list containing following fields:
#' \itemize{
#' \item{pc_1_2}{ - amount of data variance explained with each component}
#' \item{rotation}{ - rotation from \code{stats::prcomp}}
#' \item{x}{ - x from \code{stats::prcomp}}
#' \item{sdev}{ - sdev from \code{stats::prcomp}}
#' \item{label}{ - model labels}
#' }
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) - 1
#'
#' lm_model <- glm(Risk ~ .,
#'   data = german,
#'   family = binomial(link = "logit")
#' )
#'
#' rf_model <- ranger::ranger(Risk ~ .,
#'   data = german,
#'   probability = TRUE,
#'   num.trees = 200,
#'   num.threads = 1
#' )
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[, -1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'   protected = german$Sex,
#'   privileged = "male"
#' )
#'
#' # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'   protected = german$Sex,
#'   privileged = "male",
#'   cutoff = list(female = 0.4),
#'   label = c("lm_2", "rf_2")
#' )
#'
#' fpca <- fairness_pca(fobject)
#'
#' plot(fpca)
#' @export
#' @rdname fairness_pca

fairness_pca <- function(x, omit_models_with_NA = FALSE) {
  stopifnot(is.logical(omit_models_with_NA))
  stopifnot(class(x) == "fairness_object")

  # extracting metric data from object
  data <- x$parity_loss_metric_data
  labels <- x$label

  data <- data[, colnames(data) %in% parity_loss_metrics()]

  # NA handling
  if (any(is.na(data))) {
    if (omit_models_with_NA) {
      # omit models with NA
      na_model_index <- apply(data, 1, function(x) any(is.na(x)))
      models_with_missing <- as.character(labels)[na_model_index]
      warning("Found models with NA: ", models_with_missing, ", ommiting it\n")
      data <- data[!na_model_index, ]
    } else {
      # omit metrics with NA
      data <- drop_metrics_with_na(data)
    }
  }

  # throw error if after NA omit dimensions are to low
  if (nrow(data) < 2 | ncol(data) < 2) stop("Metrics data have to low dimensions\n")


  # PCA calculating
  pca_fair <- stats::prcomp(data, scale = TRUE)

  # variances
  pca_summary <- summary(pca_fair)
  pc_1_2 <- round(pca_summary$importance[2, ][1:2], 2)

  fairness_pca <- list(
    pc_1_2 = pc_1_2,
    rotation = pca_fair$rotation,
    x = pca_fair$x,
    sdev = pca_fair$sdev,
    label = x$label
  )

  class(fairness_pca) <- "fairness_pca"
  return(fairness_pca)
}
