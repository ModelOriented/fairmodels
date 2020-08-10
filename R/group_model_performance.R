#' Group model performance
#'
#' Special method for model performance evaluation. Counts number of tp, tn, fp, fn for each subgroup (and therefore potentially distinct cutoff), sums afterwards.
#'
#' @param x object created with \code{\link[DALEX]{explain}}
#' @param protected factor, vector with levels as subgroups
#' @param cutoff vector of thresholds for each subgroup
#' @param performance_metric name of performance metric
#'
#'
#' @return score in performance metric between 0 and 1
#' @export
#'
#' @rdname group_model_performance

group_model_performance <- function(x, protected, cutoff,  performance_metric){

  # group matrices for amount of tp, fn, fp, tn among groups for cutoff vector
  gm               <- group_matrices(protected,
                                     x$y_hat,
                                     x$y,
                                     cutoff)

  tp_in_gr <- lapply(gm , function(x) sum(x$tp))
  fn_in_gr <- lapply(gm , function(x) sum(x$fn))
  tn_in_gr <- lapply(gm , function(x) sum(x$tn))
  fp_in_gr <- lapply(gm , function(x) sum(x$fp))

  tp <- sum(unlist(tp_in_gr))
  fn <- sum(unlist(fn_in_gr))
  tn <- sum(unlist(tn_in_gr))
  fp <- sum(unlist(fp_in_gr))

  if (performance_metric == "recall")    mp <- model_performance_recall(tp, fp, tn, fn)
  if (performance_metric == "precision") mp <- model_performance_precision(tp, fp, tn, fn)
  if (performance_metric == "f1")        mp <- model_performance_f1(tp, fp, tn, fn)
  if (performance_metric == "accuracy")  mp <- model_performance_accuracy(tp, fp, tn, fn)

  return(mp)
}

# from DALEX
model_performance_recall <- function(tp, fp, tn, fn) {
  tp/(tp + fn)
}

model_performance_precision <- function(tp, fp, tn, fn) {
  tp/(tp + fp)
}

model_performance_f1 <- function(tp, fp, tn, fn) {
  recall = tp/(tp + fn)
  precision = tp/(tp + fp)
  2 * (precision * recall)/(precision + recall)
}

model_performance_accuracy <- function(tp, fp, tn, fn) {
  (tp + tn)/(tp + fp + tn + fn)
}
