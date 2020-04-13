#' Plot two models
#'
#' @param fairness_object object of class \code{fairness_object}
#' @param fairness_metric fairness metric name from
#' \itemize{
#' \item equal_odds
#' \item pred_rate_parity
#' \item acc_parity
#' \item fnr_parity
#' \item fpr_parity
#' \item npv_parity
#' \item spec_parity
#' \item mcc_parity
#' \item model labels
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
#' @import fairness
#' @import patchwork
#'
#' @examples
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
#' plot_two_models(fo, fairness_metric = "fnr_parity", performance_metric = "auc")
#'
#'
#' @return
#' @export
#' @rdname plot_two_models
#'

plot_two_models <- function(fairness_object , fairness_metric = NULL, performance_metric = NULL){

  stopifnot("fairness_object" %in% class(fairness_object))
  if (length(fairness_object$explainers) != 2) stop("\nNumber of explainers should be 2 for this function\n")

  x <- fairness_object$explainers[[1]]
  y <- fairness_object$explainers[[2]]


  # extract base from data (from equal_odds)
  tmp <- unlist(fairness_object$groups_data[[1]]$equal_odds)
  base <- names(tmp[tmp == 1])

  # extract row labels
  row_names <- names(tmp)

  if (is.null(fairness_metric)) {
    fairness_metric = "acc_parity"
    cat("Fairness Metric is NULL, setting deafult (", crayon::green(fairness_metric),")  \n")
  }

  if (is.null(performance_metric)) {
    performance_metric = "auc"
    cat("Performace metric is NULL, setting deafult (", crayon::green(performance_metric),")  \n")
  }

  # output for creating object
  cat("\nCreating object with: \nFairness metric", crayon::cyan(fairness_metric),
      "\nPerformance metric ", crayon::cyan(performance_metric), "\n")


  m <- ncol(data)

  # setting model info (temporary)
  x$model_info$type <- "classification"
  y$model_info$type <- "classification"

  # getting data
  from_x <- unlist(fairness_object$groups_data[[1]][fairness_metric])
  from_y <- unlist(fairness_object$groups_data[[2]][fairness_metric])

  names(from_x) <- NULL
  names(from_y) <- NULL

  fairness_data <- data.frame(group = rep(row_names,2), value = c(from_x,from_y ))
  rownames(fairness_data) <- NULL


  fairness_data <- cbind(fairness_data,
                     c( rep(x$label, length(from_x)), # x labels
                        rep(y$label, length(from_y)))) # y labesl

  colnames(fairness_data)[3] <-"order"


  levels(fairness_data$order) <- c(levels(fairness_data$order), "base") # adding base
  fairness_data <- fairness_data[2:nrow(fairness_data),]
  fairness_data <- fairness_data[order(-fairness_data$value),]


  fairness_data[fairness_data$group == base,]$order <- "base"


  cutoff <- fairness_object$cutoff
  perf_val_1 <- model_performance(x, cutoff = cutoff)$measures[performance_metric]
  perf_val_2 <- model_performance(y, cutoff = cutoff)$measures[performance_metric]

  names(perf_val_1) <- NULL
  names(perf_val_2) <- NULL
  perf_val_1 <- perf_val_1[[1]]
  perf_val_2 <- perf_val_2[[1]]


  performance_data <- data.frame(x = c(x$lab, y$lab), y = c(perf_val_1, perf_val_2))

  labels <- c(x$label, y$label)

  print(performance_metric)
    # Don't touch order here!

  fairness_data$to_vjust <- rep(0,nrow(fairness_data))
  for (i in seq_along(fairness_data$group)){
    # if is lowest in it's group than 1, else 0
    fairness_data$to_vjust[i] <- ifelse(fairness_data[i,"value"] == min(fairness_data[fairness_data$group == fairness_data$group[i],"value"]), 1.5, -0.5)
  }



  plot1 <- ggplot(fairness_data, aes(group, value, fill = order)) +
    geom_bar(stat="identity", position = "identity")  +
    geom_text(aes(label=round(value,2)),
              vjust = fairness_data$to_vjust,
              color="black",
              size=3 ,
              fontface = "bold") +
    theme_drwhy()+
    theme(axis.text.x=element_text(angle=70, hjust=0.7),
          legend.position = "none") +
    ylab(fairness_metric) +
    xlab("Models metrics in groups") +
    scale_fill_manual(values=c("#9a53c9", "#86aff0", "gray")) +
    ggtitle("2 models plot", subtitle = paste("Created with",x$labels[1],"and", x$labels[2]))

  plot2 <- ggplot(performance_data, aes(x,y, fill = x)) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(aes(label=round(y,3)), vjust=-1, color="black", size=3, fontface = "bold") +
    ylab("Accuracy") +
    theme_drwhy() +
    theme(legend.title = element_blank(),
          axis.text.x=element_text(angle=0, hjust=0.3)) +
    scale_fill_manual(values=c("#9a53c9", "#789ffa")) +
    scale_y_continuous(limits = c(0,1))+
    xlab("Models") +
    ylab(performance_metric)

  plot1 + plot2
}


