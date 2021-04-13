#' Fairness heatmap
#'
#' Create \code{fairness_heatmap} object to compare both models and metrics.
#' If parameter \code{scale} is set to \code{TRUE} metrics will be scaled to median = 0 and sd = 1.
#' If NA's appear heatmap will still plot, but with gray area where NA's were.
#'
#' @param x object of class \code{fairness_object}
#' @param scale logical, if code{TRUE} metrics will be scaled to mean 0 and sd 1. Default \code{FALSE}
#'
#' @return \code{fairness_heatmap} object.
#'
#' It is a list with following fields:
#' \itemize{
#' \item{heatmap_data}{ - \code{data.frame} with information about score for model and parity loss metric}
#' \item{matrix_model}{ - matrix used in dendogram plots}
#' \item{scale}{ - logical parameter passed to \code{fairness_heatmap}}
#' \item{label}{ - character, vector of model labels}
#' }
#' @export
#' @rdname fairness_heatmap
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#'  # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = list(female = 0.4),
#'                           label = c("lm_2", "rf_2"))
#'
#'
#' fh <- fairness_heatmap(fobject)
#'
#' plot(fh)
#'


fairness_heatmap <- function(x, scale = FALSE){

  stopifnot(class(x) == "fairness_object")
  if (length(x$explainers) < 2 ) stop("Number of explainers must be more than 1")

  # expanding data to fir geom_tile convention
  data <- expand_fairness_object(x, scale = scale)
  data <- as.data.frame(data)

  colnames(data) <- c("parity_loss_metric","model","score")

  data$parity_loss_metric <- as.factor(data$parity_loss_metric)
  data$model  <- as.factor(data$model)
  data$score  <- round(as.numeric(data$score),2)

  # getting numerical data and if scale, scaling it
  matrix_model            <- as.matrix(x$parity_loss_metric_data)
  if (scale) matrix_model <- scale(matrix_model)

  rownames(matrix_model)  <- x$label

  fairness_heatmap <- list(heatmap_data    = data,
                           matrix_model    = matrix_model,
                           scale           = scale,
                           label           = x$label)

  class(fairness_heatmap) <- "fairness_heatmap"
  return(fairness_heatmap)
}







