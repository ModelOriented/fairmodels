#' Calculate fairness metrics in groups
#'
#' @param x object of class \code{group_matrices}
#'
#' @return
#' @export
#'

calculate_group_fairness_metrics <- function(x){
  stopifnot( "group_matrices" %in% class(x) )

  group_metric_matrix <- matrix(0, nrow = 12 , ncol = length(x))
  colnames(group_metric_matrix) <- names(x)
  rownames(group_metric_matrix) <- c("TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","TS","ACC","F1", "MCC")

  for (i in seq_along(x)){
    subgroup_cm <- x[[i]]

    tp <- subgroup_cm$tp
    tn <- subgroup_cm$tn
    fp <- subgroup_cm$fp
    fn <- subgroup_cm$fn

    TPR <- tp/(tp + fn)
    TNR <- tn/(tn + fp)
    PPV <- tp/(tp + fp)
    NPV <- tn/(tn + fn)
    FNR <- fn/(fn + tp)
    FPR <- fp/(fp + tn)
    FDR <- fp/(fp + tp)
    FOR <- fn/(fn+ tn)
    TS  <- tp/(tp + fn + fp)

    # cummulated metrics
    ACC <- (tp + tn) / (tp + tn + fn + fp)
    F1  <- 2 * PPV*TPR/(PPV + TPR)

    m <- sqrt(tp+fp)*sqrt(tp+fn)*sqrt(tn+fp)*sqrt(tn+fn)
    MCC <- (tp*tn - fp * fn)/m

    group_metric_matrix[,i] <- c(TPR,TNR,PPV,NPV,FNR,FPR,FDR,FOR,TS,ACC,F1, MCC)

  }

  if (sum(is.nan(group_metric_matrix))){
    message("NA's created in metric's matrix ")
    group_metric_matrix[is.nan(group_metric_matrix)] <- NA
  }

  class(group_metric_matrix) <- "group_metric_matrix"
  return(group_metric_matrix)
}
