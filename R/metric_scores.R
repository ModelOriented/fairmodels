#' Metric scores
#'
#' Creates \code{metric_scores} object to facilitate visualization. Check how the metric scores differ among models, what is this score, and how it changes
#' for example after applying bias mitigation technique. The vertical black lines
#' denote the scores for privileged subgroup. It is best to use only few metrics (using \code{fairness_metrics} parameter)
#'
#' @param x object of class \code{fairness_object}
#' @param fairness_metrics character, vector with fairness metric names. Default metrics are ones in \code{fairness_check} plot, full names can be found in \code{fairness_check} documentation.
#'
#' @return \code{metric_scores} object.
#' It is a list containing:
#' \itemize{
#' \item{metric_scores_data}{ - \code{data.frame} with information about score in particular subgroup, metric, and model}
#' \item{privileged}{ - name of privileged subgroup}
#' }
#' @export
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
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' ms <- metric_scores(fobject, fairness_metrics = c('ACC', 'TPR', 'PPV', 'FPR', 'STP'))
#' plot(ms)
#'



metric_scores <- function(x, fairness_metrics = c('ACC', 'TPR', 'PPV', 'FPR', 'STP')){
  stopifnot(class(x) == "fairness_object")

  if (! is.character(fairness_metrics)) stop("fairness_metrics must be character vector")
  sapply(fairness_metrics, assert_parity_metrics)

  data <- data.frame()
  for (model in names(x$groups_data)){
    model_data_frame <- data.frame()
    model_data       <- lapply(x$groups_data[[model]],
                               function(x) data.frame(score = x,
                                                      subgroup = names(x)))
    for (i in seq_along(model_data)){
      model_data[[i]]$metric <- names(model_data)[i]
      model_data_frame <- rbind(model_data_frame, model_data[[i]])
    }

    model_data_frame$model     <- model
    rownames(model_data_frame) <- NULL
    data <- rbind(data, model_data_frame)
  }

  data$model <- as.factor(data$model)
  data <- data[data$metric %in% fairness_metrics,]

  metric_scores <- list(metric_scores_data = data,
                        privileged = x$privileged)
  class(metric_scores) <- "metric_scores"
  return(metric_scores)
}
