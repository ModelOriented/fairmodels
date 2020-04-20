#' Group confusion matrices
#'
#' @param data data frame
#' @param group \code{character} name of column with group
#' @param probs \code{character} name of column with probabilities
#' @param outcome \code{character} name of column with outcome
#' @param outcome_numeric \code{numeric} vector of outcome
#' @param cutoff \code{numeric} cutoff for probabilities, deafult = 0.5
#'
#' @return
#' @export
#' @rdname group_metrices


group_matrices <- function(data, group, probs = "probabilities", outcome, outcome_numeric, cutoff){

  data$outcome_numeric = outcome_numeric

  if(!is.factor(data[,group])) stop("\ndata[,group] is not factor\n")
  if(!true_value %in% data$outcome_numeric) stop("true_value not in outcome\n")

  false_value <- unique(data$outcome_numeric)
  false_value <- false_value[false_value != true_value]

  group_levels <- as.character(unique(data[,group]))

  group_confusion_metrices <- list()

  for (i in seq_along(group_levels)){
    subgroup <- group_levels[i]
    sub_data <- data[data[,group] == subgroup,]

    true_values <- sub_data$outcome_numeric

    cm <- confusion_matrix(sub_data[,probs], true_values,cutoff = cutoff)

    if (cm$tp == 0 | cm$fp == 0 | cm$tn == 0 | cm$fn == 0) message("\n0's appear in confusion matrix for group: ", subgroup )

    group_confusion_metrices[[i]] <- cm
    names(group_confusion_metrices)[i] <- subgroup
  }
  class(group_confusion_metrices) <- "group_matrices"
  return(group_confusion_metrices)
}

