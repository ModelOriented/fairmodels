#' Two models plot
#'
#' @param x explainer 1
#' @param y explainer 2
#' @param outcome target variable
#' @param group protected group/ variable
#' @param base threshold to which metrics will be compared
#' @param data data frame, if null getting data from \code{x}
#' @param fairness_metric fairness metric name
#' @param performance_metric performance metric name
#' @param cutoff deafult = 0.5
#'
#' @import DALEX
#' @import fairness
#' @import crayon
#'
#' @return
#' @export
#' @rdname two_models

two_models <- function(x, y, outcome, group, base, data = NULL, fairness_metric = NULL, performance_metric = NULL, cutoff = 0.5 ){


  if (is.null(data)) {
    data = x$data
    cat("Getting data from first (", green(x$label),")  explainer \n")
  }

  if (is.null(fairness_metric)) {
    fairness_metric = "acc_parity"
    cat("Fairness Metric is NULL, setting deafult (", green(fairness_metric),")  \n")
  }


  if (is.null(performance_metric)) {
    performance_metric = "auc"
    cat("Performace metric is NULL, setting deafult (", green(performance_metric),")  \n")
  }

  # output for creating object
  cat("\nCreating object with: \nFairness metric", cyan(fairness_metric),
      "\nPerformance metric ", cyan(performance_metric), "\n")


  fairness_object_list <- list(0,0)
  m <- ncol(data)

  # setting model info (temporary)
  x$model_info$type <- "classification"
  y$model_info$type <- "classification"

  explainers <- list(x,y)   # iterating over explainers

  # extracting performances
  for (i in seq_along(explainers)){

    # for each predictions we look at whole data
    df[,m+1] <- explainers[[i]]$y_hat
    colnames(df)[m+1] <- "probabilities"

    # creating fairness object
    fairness_arguments <- list(data = df ,
                               outcome = outcome,
                               group = group,
                               base = base,
                               probs = "probabilities")

    # and adding it to fainress list
    fairness_object_list[[i]] <- do.call(fairness_metric, args = fairness_arguments)
  }

  # getting data
  from_x <- fairness_object_list[[1]]$Metric[2,]
  from_y <- fairness_object_list[[2]]$Metric[2,]

  perf_fair <- data.frame(  x= rep(names(fairness_object_list[[1]]$Metric[1,]),2), # names
                            y = c(from_x,from_y))

  perf_fair <- cbind(perf_fair,c(rep("first", length(fairness_object_list[[1]]$Metric[1,])),
                     rep("second", length(fairness_object_list[[2]]$Metric[2,]))) )

  colnames(perf_fair)[3] <- "order"
  perf_fair <- perf_fair[2:nrow(perf_fair),]
  perf_fair <- perf_fair[order(-perf_fair$y),]

  perf_fair[perf_fair$x == base,]$order <- "first"

  perf_val_1 <- model_performance(x, cutoff = cutoff)$measures[performance_metric]
  perf_val_2 <- model_performance(y, cutoff = cutoff)$measures[performance_metric]

  # clean
  names(perf_val_1) <- NULL
  names(perf_val_2) <- NULL
  perf_val_1 <- perf_val_1[[1]]
  perf_val_2 <- perf_val_2[[1]]


  perf_mod <- data.frame(x = c("first", "second"), y = c(perf_val_1, perf_val_2))


  two_models <- list(fairness = perf_fair, performance = perf_mod, fairness_metric = fairness_metric,  performance_metric = performance_metric)
  class(two_models) <- "two_models"

  two_models
}
