#' Group metric
#'
#' @description Group metric enables to extract data from metrics generated for each group, and prepare it for visualization.
#'
#' @param x object of class \code{fairness_object}
#' @param fairness_metric fairness metric name
#' @param performance_metric performance metric name
#' @param parity_loss logical, if TRUE parity loss will supersede normal metirc
#'
#' @details
#' Where:
#'
#' Fairness metrics:
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
#' \item F1
#' \item MCC
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
#' @import DALEX
#' @import patchwork
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#' data("compas")
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' rf_1 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' lr_1 <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' explainer_1 <- explain(rf_1, data = compas, y = y_numeric)
#' explainer_2 <- explain(lr_1, data = compas, y = y_numeric)
#'
#' fo <- create_fairness_object(explainer_1, explainer_2,  outcome = "Two_yr_Recidivism", group = "Ethnicity", base = "Caucasian"  )
#'
#' gm <- group_metric(fo, fairness_metric = "FPR", performance_metric = "auc")
#' plot(gm)
#'
#' @return \code{groum_metric} object
#' @export
#' @rdname group_metric
#'

group_metric <- function(x , fairness_metric = NULL, performance_metric = NULL, parity_loss = FALSE){

  stopifnot(class(x) == "fairness_object")
  stopifnot(is.logical(parity_loss))

  fairness_object <- x
  base  <- fairness_object$base
  data  <- fairness_object$data
  m     <- ncol(data)
  n     <- length(fairness_object$groups_data[[1]][[1]])
  n_exp <- length(fairness_object$explainers)

  if (is.null(fairness_metric)) {
    fairness_metric <-  "ACC"
    cat("Fairness Metric not given, setting deafult (", fairness_metric,")  \n")
  }

  if (is.null(performance_metric)) {
    performance_metric <-  "auc"
    cat("Performace metric not given, setting deafult (", performance_metric,")  \n")
  }


  assert_base_metrics(fairness_metric)
  assert_performance_metrics(performance_metric)


  # output for creating object
  cat("\nCreating object with: \nFairness metric", fairness_metric,
      "\nPerformance metric ", performance_metric, "\n")


  # Fairness metric
  group_data <- list()
  labels     <- list()

  for (i in seq_len(n_exp)){
    group_data[[i]] <- fairness_object$groups_data[[i]][fairness_metric][[1]]

    # if parity loss, then scale
    if (parity_loss) group_data[[i]] <- abs(group_data[[i]] - group_data[[i]][base] )

    labels[[i]]       <- fairness_object$explainers[[i]]$label[[1]]
  }

  unlisted_group_data <- unlist(group_data)
  row_names           <- names(unlisted_group_data)
  labels              <- unlist(labels)
  labels_rep          <- rep(labels, each = n)

  fairness_data <- data.frame(group = row_names, value = unlisted_group_data, label = labels_rep)

  # performance metric
  cutoff  <- fairness_object$cutoff
  model_perf <- list()

  for (i in seq_len(n_exp)){
    exp <- fairness_object$explainers[[i]]
    model_perf[[i]]   <- model_performance(exp, cutoff = cutoff[i])$measures[performance_metric][[1]]

  }

  model_perf       <- unlist(model_perf)
  performance_data <- data.frame(x = labels, y = model_perf)

  y_label <- fairness_metric
  if (parity_loss) y_label <- paste0(y_label, "_parity_loss")


  group_metric <-  list(fairness_data = fairness_data,
                        performance_data = performance_data,
                        y_label = y_label,
                        performance_metric = performance_metric)
  class(group_metric) <- "group_metric"
  return(group_metric)
}





