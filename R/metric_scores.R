#' Metric scores
#'
#' @description creates metric_scores object to facilitate visualization. Check how the metric scores differ among models, what is this score, and how it changes
#' for example after applying bias mitigation technique. The vertical black lines
#' denotes the score for privileged subgroup. It is best to use only few metrics (using \code{fairness_metrics} parameter)
#'
#' @param x \code{fairness_object} object
#' @param fairness_metrics character, vector with fairness metric names
#'
#' @return \code{metric_scores} object
#' @export
#'
#' @examples
#'
#' data("compas")
#'
#' # flipping outcomes, 1- favorable
#' compas$Two_yr_Recidivism <- as.factor(ifelse(compas$Two_yr_Recidivism == '1', '0', '1'))
#'
#' # train
#' lm_compas <- glm(Two_yr_Recidivism ~., data = compas, family = binomial())
#' rf_compas <- ranger::ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#'
#' # numeric target values
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' # explainer
#' rf_explainer <- DALEX::explain(rf_compas, data = compas[,-1], y = y_numeric)
#' lm_explainer <- DALEX::explain(lm_compas, data = compas[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(rf_explainer, lm_explainer,
#'                           protected = compas$Ethnicity,
#'                           privileged = "Caucasian")
#'
#' ms <- metric_scores(fobject, fairness_metrics = c("TPR","STP","ACC"))
#' plot(ms)
#'



metric_scores <- function(x, fairness_metrics = unique_metrics()){
  stopifnot(class(x) == "fairness_object")

  if (! is.character(fairness_metrics)) stop("fairness_metrics must be character vector")
  sapply(fairness_metrics, assert_parity_metrics)

  data <- data.frame()
  for (model in names(x$groups_data)){
    model_data_frame <- data.frame()
    model_data           <- lapply(x$groups_data[[model]], function(x) data.frame(score = x, subgroup = names(x)))
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


