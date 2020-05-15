#' Fairness radar
#'
#' @description Make fairness_radar object whith chosen metrics. Note that there must be at least three metrics that does not contain NA.
#'
#' @param x \code{fairness_object}
#' @param metrics vector of metric names, at least 3 metrics without NA needed. If \code{NULL} deafult metrics will be taken
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


fairness_radar <- function(x, metrics = NULL){

  stopifnot(class(x) == "fairness_object")

  data <- x$metric_data
  m <- ncol(data)

  available_metrics <- colnames(data[,1:(m-1)])

  # metrics
  if (is.null(metrics)) metrics <- unique_metrics()
  if (! is.character(metrics) ) stop("metric argument must be character metric")
  if (! all(metrics %in% available_metrics)) stop("in metric argument there are metrics not in fairness_object")
  if (length(metrics) < 3 ) stop("Number of metrics in radar plot must be at least 3")

  # NA handling
  if (any(is.na(data))){
    data              <- data
    na_col_index      <- apply(data, 2, function(x) any(is.na(x)))
    cols_with_missing <- names(data)[na_col_index]
    warning("Found metric with NA: ", cols_with_missing, ", ommiting it")


    metrics <- metrics[! metrics %in% cols_with_missing]
  }

  df <- expand_fairness_object(x)

  # taking only some metrics
  df <- df[df$metric %in% metrics,]

  if (dim(df)[1] <= 0) stop("metric data has no rows")
  if (dim(df)[2] <= 2) stop("metric data must have at least 3 columns")

  n  <- length(x$explainers)

  # ordering metrics
  df            <- df[order(df$metric),]
  rownames(df)  <- NULL
  levels_sorted <- levels(sort(df$metric))
  df$metric     <- factor(df$metric, levels = levels_sorted )

  fairness_radar <- list(df = df ,
                         n = n)

  class(fairness_radar) <- "fairness_radar"
  return(fairness_radar)

}

