#' Fairness radar
#'
#' @description Make fairness_radar object whith chosen fairness_metrics. Note that there must be at least three metrics that does not contain NA.
#'
#' @param x \code{fairness_object}
#' @param fairness_metrics character, vector of metric names, at least 3 metrics without NA needed. If \code{NULL} deafult metrics will be taken.
#'
#' @return \code{fairness_radar} object
#' @export
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
#' fobject <- create_fairness_object(explainer_glm, explainer_rf,
#'                              outcome = "Two_yr_Recidivism",
#'                              group = "Ethnicity",
#'                              base = "Caucasian",
#'                              cutoff = 0.5)
#' fradar <- fairness_radar(fobject)
#' plot(fradar)


fairness_radar <- function(x, fairness_metrics = NULL){

  stopifnot(class(x) == "fairness_object")

  data <- x$metric_data
  m <- ncol(data)


  # metrics
  if (is.null(fairness_metrics)) fairness_metrics <- unique_metrics()
  if (! is.character(fairness_metrics) ) stop("metric argument must be character metric")
  sapply(fairness_metrics,assert_parity_metrics)

  if (length(fairness_metrics) < 3 ) stop("Number of metrics in radar plot must be at least 3")

  # NA handling
  if (any(is.na(data))){
    data              <- data
    na_col_index      <- apply(data, 2, function(x) any(is.na(x)))
    cols_with_missing <- names(data)[na_col_index]
    warning("Found metric with NA: ", cols_with_missing, ", ommiting it")


    fairness_metrics <- fairness_metrics[! fairness_metrics %in% cols_with_missing]
  }

  expanded_data <- expand_fairness_object(x)

  # taking only some metrics
  expanded_data <- expanded_data[expanded_data$metric %in% fairness_metrics,]

  if (dim(expanded_data)[1] <= 0) stop("metric data has no rows")
  if (dim(expanded_data)[2] <= 2) stop("metric data must have at least 3 columns")

  n_exp  <- length(x$explainers)

  # ordering metrics
  expanded_data            <- expanded_data[order(expanded_data$metric),]
  rownames(expanded_data)  <- NULL
  levels_sorted            <- levels(sort(expanded_data$metric))
  expanded_data$metric     <- factor(expanded_data$metric, levels = levels_sorted )

  fairness_radar <- list(df = expanded_data ,
                         n = n_exp)

  class(fairness_radar) <- "fairness_radar"
  return(fairness_radar)

}

