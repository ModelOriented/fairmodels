#' Group metric
#'
#' Group metric enables to extract data from metrics generated for each subgroup (values in protected variable)
#' The closer metric values are to each other, the less bias particular model has. If \code{parity_loss} parameter is set to \code{TRUE}, distance between
#' privileged and unprivileged subgroups will be measured. When plotted shows both fairness metric and chosen performance metric.
#'
#' @param x object of class \code{fairness_object}
#' @param fairness_metric character, fairness metric name, if \code{NULL} the default metric will be used which is TPR.
#' @param performance_metric character, performance metric name
#' @param parity_loss logical, if \code{TRUE} parity loss will supersede basic metric
#' @param verbose logical, whether to print information about metrics on console or not. Default \code{TRUE}
#'
#' @importFrom DALEX model_performance
#'
#' @details
#' Available metrics:
#'
#' Fairness metrics (Full names explained in \code{fairness_check} documentation):
#'
#' \itemize{
#' \item TPR
#' \item TNR
#' \item PPV
#' \item NPV
#' \item FNR
#' \item FPR
#' \item FDR
#' \item FOR
#' \item TS
#' \item ACC
#' \item STP
#' \item F1
#' }
#' Performance metrics
#'
#' \itemize{
#' \item recall
#' \item precision
#' \item accuracy
#' \item f1
#' \item auc
#' }
#'
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
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' gm <- group_metric(fobject, "TPR", "f1", parity_loss = TRUE)
#' plot(gm)
#'
#' \donttest{
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#' gm <- group_metric(fobject, "TPR", "f1", parity_loss = TRUE)
#'
#' plot(gm)
#'
#'}
#'
#' @return \code{group_metric} object.
#' It is a list with following items:
#' \itemize{
#' \item{group_metric_data}{ - \code{data.frame} containing fairness metric scores for each model}
#' \item{performance_data}{ - \code{data.frame} containing performance metric scores for each model}
#' \item{fairness_metric}{ - name of fairness metric}
#' \item{performance_metric}{ - name of performance metric}
#'
#' }
#'
#' @export
#' @rdname group_metric
#'

group_metric <- function(x, fairness_metric = NULL, performance_metric = NULL, parity_loss = FALSE, verbose = TRUE){

  stopifnot(class(x) == "fairness_object")
  stopifnot(is.logical(parity_loss))

  base  <- x$privileged
  n     <- length(x$groups_data[[1]][[1]])
  n_exp <- length(x$explainers)

  if (is.null(fairness_metric)) {
    fairness_metric <-  "TPR"
    verbose_cat("Fairness Metric not given, setting deafult (", fairness_metric,")  \n", verbose = verbose)
  }

  if (is.null(performance_metric)) {
    performance_metric <-  "accuracy"
    verbose_cat("Performace metric not given, setting deafult (", performance_metric,")  \n", verbose = verbose)
  }


  assert_base_metrics(fairness_metric)
  assert_performance_metrics(performance_metric)


  # output for creating object
  verbose_cat("\nCreating object with: \nFairness metric: ", fairness_metric,
      "\nPerformance metric: ", performance_metric, "\n\n", verbose = verbose)


  # Fairness metric
  group_data <- list()
  labels     <- list()

  for (i in seq_len(n_exp)){
    group_data[[i]] <- x$groups_data[[i]][fairness_metric][[1]]

    # if parity loss, then scale
    if (parity_loss) group_data[[i]] <- abs(group_data[[i]] - group_data[[i]][base] )

    labels[[i]]       <- x$label[i]
  }

  unlisted_group_data <- unlist(group_data)
  row_names           <- names(unlisted_group_data)
  labels              <- unlist(labels)
  labels_rep          <- rep(labels, each = n)

  group_metric_data <- data.frame(group = row_names,
                              score = unlisted_group_data,
                              model = labels_rep)

  # performance metric
  cutoff   <- x$cutoff
  mod_perf <- rep(0, length(x$explainers))

  for (i in seq_len(n_exp)){

    if (performance_metric == "auc"){
      mod_perf[i]  <- model_performance(x$explainers[[i]])$measures[performance_metric][[1]]

    } else {
      # if else use custom cutoff function implemented in fairmodels
      mod_perf[i] <- group_model_performance(x$explainers[[i]],
                                             protected  = x$protected,
                                             cutoff     = x$cutoff[[i]],
                                             performance_metric = performance_metric)
    }

  }

  performance_data <- data.frame(model = x$label, score = mod_perf)

  if (parity_loss){
    fairness_metric <- paste0(fairness_metric, " parity loss")
    group_metric_data <- group_metric_data[group_metric_data$group != x$privileged,]
  }

  group_metric <-  list(group_metric_data  = group_metric_data,
                        performance_data   = performance_data,
                        fairness_metric    = fairness_metric,
                        performance_metric = performance_metric,
                        label = x$label)

  class(group_metric) <- "group_metric"
  return(group_metric)
}
