#' Print all cutoffs
#'
#' @param x \code{all_cuttofs} object
#' @param ... other print parameters
#' @param label character, label of model to plot. Default NULL. If default prints all models.
#'
#' @export
#'
#' @importFrom utils head
#'
#' @rdname print_all_cutoffs
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' ac <- all_cutoffs(fobject,
#'                   fairness_metrics = c("TPR",
#'                                        "FPR"))
#' print(ac)
#'

print.all_cutoffs <- function(x, ..., label = NULL){

  if (is.null(label)){
    data <- x$cutoff_data
  } else {
    if (! is.character(label) | length(label) > 1)  stop("label must be character")
    data <- x$cutoff_data[x$cutoff_data$label == label, ]
  }

  label <- unique(data$label)

  cat("\nAll cutofs for models:\n", paste(label, collapse = ", "), "\n")
  cat("\nFirst rows from data: \n")
  print(head(data), ...)

  cat("\n")
  return(invisible(NULL))
}

################################################################################

#' Print ceteris paribus cutoff
#'
#' @param x \code{ceteris_paribus_cutoff} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_ceteris_paribus_cutoff
#'
#' @examples
#'
#' data("german")
#'
#' german <- german[1:500,]
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' ceteris_paribus_cutoff(fobject, "female")


print.ceteris_paribus_cutoff<- function(x, ...){

  data <- x$cutoff_data

  cat("\nCeteribus paribus cutoff for subgroup:", x$subgroup, "\n")
  cat("\nFirst rows from data: \n")
  print(head(data), ...)
  cat("\nMinimums: \n")
  print(x$min_data, ...)
  cat("\n")
  return(invisible(NULL))
}

################################################################################

#' Print chosen metric
#'
#' @description Choose metric from parity loss metrics and plot it for every model.
#' The one with the least parity loss is more fair in terms of this particular metric.
#'
#' @param x \code{chosen_metric} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_chosen_metric
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' cm <- choose_metric(fobject, "TPR")
#' print(cm)

print.chosen_metric <- function(x,...){

  data <- x$parity_loss_metric_data

  cat("\nchoosen metric:\n", x$metric)
  cat("\ndata:\n")
  print(data, ...)

  cat("\n")
  return(invisible(NULL))

}

################################################################################

#' Print fairness heatmap
#'
#' @param x \code{fairness_heatmap} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_fairness_heatmap
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#'  # same explainers with different cutoffs for female
#'  fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = list( female = 0.4),
#'                           label = c("lm_2", "rf_2"))
#'
#'
#' fh <- fairness_heatmap(fobject)
#' print(fh)
#'

print.fairness_heatmap <- function(x, ...) {

  data         <- x$heatmap_data
  matrix_model <- x$matrix_model

  scaled <- x$scale
  cat("heatmap data top rows: \n")
  print(head(data, 5), ...)
  cat("\n")

  cat("matrix model", ifelse(scaled, "scaled", "not scaled"), ":\n")
  print(matrix_model, ...)


  cat("\n")
}

################################################################################

#' Print Fairness Object
#'
#'
#' @param x \code{fairness_object} object
#' @param ... other parameters
#' @param colorize logical, whether information about metrics should be in color or not
#'
#' @importFrom utils head
#' @importFrom stats na.omit
#'
#' @export
#' @rdname print_fairness_object
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
#'                            max.depth = 3,
#'                            num.trees = 100,
#'                            seed = 1,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' explainer_rf <- DALEX::explain(rf_model,
#'                                data = german[,-1],
#'                                y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' print(fobject)
#'
#'


print.fairness_object <- function(x, ..., colorize = TRUE){

  if (! colorize) {
    color_codes <- list(yellow_start = "", yellow_end = "",
                        red_start = "", red_end = "",
                        green_start = "", green_end = "")
  }


  data <- x$fairness_check_data

  models  <- unique(data$model)
  epsilon <- x$epsilon
  metrics <- unique(data$metric)


  if (any(is.na(data$score))){

    warning("Omiting NA for models: ",
            paste(unique(data[is.na(data$score), "model"]),
                  collapse = ", "),
            "\nInformation about passed metrics may be inaccurate due to NA present, it is advisable to check metric_scores plot.\n")
  }



  cat("\nFairness check for models:", paste(models, collapse = ", "), "\n")

  for (model in models){
    model_data <- data[data$model == model,]

    failed_metrics <- unique(model_data[na.omit(model_data$score) < epsilon | na.omit(model_data$score) > 1/epsilon, "metric"])
    passed_metrics <-  length(metrics[! metrics %in% failed_metrics])

    if (passed_metrics < 4){
      cat("\n", color_codes$red_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$red_end ,  sep = "")}
    if (passed_metrics == 4){
      cat("\n", color_codes$yellow_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$yellow_end ,  sep = "")
    }
    if (passed_metrics == 5){
      cat("\n", color_codes$green_start ,model, " passes ", passed_metrics, "/5 metrics\n", color_codes$green_end ,  sep = "")}

    cat("Total loss: ", sum(abs(na.omit(data[data$model == model, "score" ])- 1)), "\n")
  }

  cat("\n")
  return(invisible(NULL))

}

################################################################################

#' Print fairness PCA
#'
#' @description Print principal components after using pca on fairness object
#'
#' @param x \code{fairness_pca} object
#' @param ... other print parameters
#'
#'
#' @export
#' @rdname print_fairness_pca
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
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#'  # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = list( female = 0.4),
#'                           label = c("lm_2", "rf_2"))
#'
#' fpca <- fairness_pca(fobject)
#'
#' print(fpca)
#'


print.fairness_pca <- function(x, ...){

  cat("Fairness PCA : \n")
  print(x$x, ...)

  cat("\nCreated with: \n")
  print(as.character(x$label), ...)

  cat("\nFirst two components explained", sum(x$pc_1_2)*100, "% of variance.\n")

  return(invisible(NULL))
}

################################################################################

#' Print fairness radar
#'
#' @param x \code{fairness_radar} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_fairness_radar
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#'
#' fradar <- fairness_radar(fobject)
#'
#' print(fradar)
#'
print.fairness_radar <- function(x, ...){

  data  <- x$radar_data

  cat("\nFairness radar for: ", paste(unique(data$model), collapse = ", "), "\n")

  cat("First rows from data: \n")
  print(head(data), ...)
  cat("\n")
  return(invisible(NULL))
}

################################################################################

#' Print group metric
#'
#' @param x \code{group_metric} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_group_metric
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' gm <- group_metric(fobject, "TPR", "f1", parity_loss = TRUE)
#'
#' print(gm)

print.group_metric <- function(x, ...){


  cat("Fairness data top rows for", x$fairness_metric, "\n")
  print(head(x$group_metric_data, ...))
  cat("\n")

  cat("Performance data for", x$performance_metric, ":")

  perf_df <- x$performance_data
  colnames(perf_df) <- NULL
  print(perf_df)

  cat("\n")
  return(invisible(NULL))
}

################################################################################

#' Print metric scores data
#'
#' @param x \code{metric_scores} object
#' @param ... other print parameters
#'
#' @export
#' @rdname print_metric_scores
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' ms <- metric_scores(fobject, fairness_metrics = c("TPR","STP","ACC"))
#' ms

print.metric_scores <- function(x, ...){
  data  <- x$metric_scores_data

  cat("\nMetric scores calculated for: ", paste(unique(data$model), collapse = ", "), "\n")

  cat("First rows from data: \n")
  print(head(data), ...)
  cat("\n")
  return(invisible(NULL))
}

################################################################################

#' Print performance and fairness
#'
#' @param x \code{performance_and_fairness} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#' @rdname print_performance_and_fairness
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#'  # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = list(female = 0.4),
#'                           label = c("lm_2", "rf_2"))
#'
#' paf <- performance_and_fairness(fobject)
#'
#' paf

print.performance_and_fairness <- function(x, ...){

  data               <- x$paf_data
  performance_metric <- x$performance_metric
  fairness_metric    <- x$fairness_metric

  cat("performance_and_fairness object created for: \n")
  print(x$label)

  cat("\ndata: \n")
  print(data, ...)

  return(invisible(NULL))

}

################################################################################

#' Print stacked metrics
#'
#' @description Stack metrics sums parity loss metrics for all models. Higher value of stacked metrics means the model is less fair (has higher bias)
#' for subgroups from protected vector.
#'
#' @param x \code{stacked_metrics} object
#' @param ... other print parameters
#'
#' @importFrom utils head
#'
#' @export
#'
#' @rdname print_stacked_metrics
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
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' sm <- stack_metrics(fobject)
#' print(sm)
print.stacked_metrics <- function(x, ...){

  data <- x$stacked_data

  cat("\nFirst rows of stacked data: \n")
  print(head(data, ...))
  cat("\n")
  return(invisible(NULL))
}

