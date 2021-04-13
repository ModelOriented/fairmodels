#' Fairness radar
#'
#' Make \code{fairness_radar} object with chosen \code{fairness_metrics}. Note that there must be at least three metrics that does not contain NA.
#'
#' @param x object of class \code{fairness_object}
#' @param fairness_metrics character, vector of metric names, at least 3 metrics without NA needed. Full names of metrics can be found in \code{fairness_check} documentation.
#'
#' @return \code{fairness_radar} object.
#' It is a list containing:
#' \itemize{
#' \item{radar_data}{ - \code{data.frame} containing scores for each model and parity loss metric}
#' \item{label}{ - model labels}
#' }
#' @export
#'
#' @examples
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' fradar <- fairness_radar(fobject, fairness_metrics = c("ACC", "STP", "TNR",
#'                                                        "TPR", "PPV"))
#'
#' plot(fradar)
#'
#' \donttest{
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#'
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#'
#' fradar <- fairness_radar(fobject, fairness_metrics = c("ACC",
#'                                                        "STP",
#'                                                        "TNR",
#'                                                        "TPR",
#'                                                        "PPV"))
#'
#' plot(fradar)
#' }


fairness_radar <- function(x, fairness_metrics = c('ACC', 'TPR', 'PPV', 'FPR', 'STP')){

  stopifnot(class(x) == "fairness_object")

  data <- x$parity_loss_metric_data
  m <- ncol(data)

  # metrics
  if (is.null(fairness_metrics)) fairness_metrics <- fairness_check_metrics()
  if (! is.character(fairness_metrics) ) stop("metric argument must be character metric")
  sapply(fairness_metrics, assert_parity_metrics)

  if (length(fairness_metrics) < 3 ) stop("Number of metrics in radar plot must be at least 3")

  # NA handling
  if (any(is.na(data))){
    na_col_index      <- apply(data, 2, function(x) any(is.na(x)))
    cols_with_missing <- names(data)[na_col_index]

    cols_with_missing <- cols_with_missing[cols_with_missing %in% fairness_metrics]
    if (length(cols_with_missing) > 0){
      warning("Found metric with NA: ", paste(cols_with_missing, collapse = ", "), ", ommiting it")
      fairness_metrics <- fairness_metrics[! fairness_metrics %in% cols_with_missing]
    }
  }

  expanded_data <- expand_fairness_object(x, fairness_metrics = fairness_metrics)


  if (length(unique(expanded_data$metric)) <= 2) stop("metric data must have at least 3 columns without NA")

  n_exp  <- length(x$explainers)

  # ordering metrics
  expanded_data            <- expanded_data[order(expanded_data$metric),]
  rownames(expanded_data)  <- NULL
  levels_sorted            <- levels(sort(expanded_data$metric))
  expanded_data$metric     <- factor(expanded_data$metric, levels = levels_sorted )
  colnames(expanded_data)[1]  <- "parity_loss_metric"

  fairness_radar <- list(radar_data = expanded_data ,
                         label      = x$label)

  class(fairness_radar) <- "fairness_radar"
  return(fairness_radar)

}

