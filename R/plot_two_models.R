#' Plot two models
#'
#' @param x object of class \code{fairness_object}
#' @param fairness_metric fairness metric name from
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
#' @param performance_metric performance metric name from
#' \itemize{
#' \item recall
#' \item precision
#' \item accuracy
#' \item f1
#' \item auc
#' }
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
#' plot_two_models(fo, fairness_metric = "FPR", performance_metric = "auc")
#'
#'
#' @return
#' @export
#' @rdname plot_two_models
#'

plot_two_models <- function(x , fairness_metric = NULL, performance_metric = NULL){

  fairness_object <- x

  stopifnot("fairness_object" %in% class(fairness_object))
  if (length(fairness_object$explainers) != 2) stop("\nNumber of explainers should be 2 for this function\n")

  x <- fairness_object$explainers[[1]]
  y <- fairness_object$explainers[[2]]

  base <- fairness_object$base

  if (is.null(fairness_metric)) {
    fairness_metric <-  "ACC"
    cat("Fairness Metric is NULL, setting deafult (", crayon::green(fairness_metric),")  \n")
  }

  if (is.null(performance_metric)) {
    performance_metric <-  "auc"
    cat("Performace metric is NULL, setting deafult (", crayon::green(performance_metric),")  \n")
  }

  # output for creating object
  cat("\nCreating object with: \nFairness metric", crayon::cyan(fairness_metric),
      "\nPerformance metric ", crayon::cyan(performance_metric), "\n")


  m <- ncol(data)

  # getting data
  from_x <- fairness_object$groups_data[[1]][fairness_metric][[1]]
  from_y <- fairness_object$groups_data[[2]][fairness_metric][[1]]

  row_names     <- names(from_x)
  fairness_data <- data.frame(group = rep(row_names,2), value = c(from_x,from_y ))

  fairness_data <- cbind(fairness_data,
                         c( rep(x$label, length(from_x)),  #  x labels
                         rep(y$label, length(from_y))))    #  y labesl

  colnames(fairness_data)[3] <- "label"

  cutoff <- fairness_object$cutoff
  perf_val_1 <- model_performance(x, cutoff = cutoff[1])$measures[performance_metric]
  perf_val_2 <- model_performance(y, cutoff = cutoff[2])$measures[performance_metric]

  names(perf_val_1) <- NULL
  names(perf_val_2) <- NULL
  perf_val_1 <- perf_val_1[[1]]
  perf_val_2 <- perf_val_2[[1]]


  performance_data <- data.frame(x = c(x$lab, y$lab), y = c(perf_val_1, perf_val_2))


  plot1 <- ggplot(fairness_data, aes(group, value, fill = label)) +
              geom_bar(stat="identity",
                       position = "dodge")  +
              theme_drwhy() +
              theme(axis.text.x=element_text(angle=90, hjust=1),
                    legend.position = "none") +
              ylab(fairness_metric) +
              xlab("Models metrics in groups") +
              scale_fill_manual(values=c("#8bdcbe", "#4378bf")) +
              ggtitle("2 models plot",
                      subtitle = paste("Created with",
                                       x$label, "and", y$label))

  plot2 <- ggplot(performance_data, aes(x,y, fill = x)) +
                    geom_bar(stat = "identity",
                             width = 0.4) +
                    geom_text(aes(label=round(y,3)), vjust=-1, color="black", size=3, fontface = "bold") +
                    ylab("Accuracy") +
                    theme_drwhy() +
                    theme(legend.title = element_blank(),
                          axis.text.x=element_text(angle=0, hjust=0.3)) +
                    scale_fill_manual(values=c("#8bdcbe", "#4378bf")) +
                    scale_y_continuous(limits = c(0,1))+
                    xlab("Models") +
                    ylab(performance_metric)

  plot1 + plot2
}


