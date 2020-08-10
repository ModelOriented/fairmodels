#' Calculate fairness metrics in groups
#'
#' Create \code{data.frame} from \code{group_matrices} object containing metric scores for each subgroup.
#'
#' @param x object of class \code{group_matrices}
#'
#' @return \code{group_metric_matrix} object
#' It's a \code{data.frame} with metrics as row names and scores for those metrics for each subgroup in columns
#' @export
#'

calculate_group_fairness_metrics <- function(x){
  stopifnot( "group_matrices" %in% class(x) )

  group_metric_matrix <- matrix(0, nrow = 13 , ncol = length(x))
  colnames(group_metric_matrix) <- names(x)
  rownames(group_metric_matrix) <- c("TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","TS","STP","ACC","F1", "MCC")

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

    # accumulated metrics
    STP <- (tp + fp) /(tp + fp + tn + fn)
    ACC <- (tp + tn) / (tp + tn + fn + fp)
    F1  <- 2 * PPV*TPR/(PPV + TPR)

    m <- sqrt(tp+fp)*sqrt(tp+fn)*sqrt(tn+fp)*sqrt(tn+fn)
    MCC <- (tp*tn - fp * fn)/m

    group_metric_matrix[,i] <- c(TPR,TNR,PPV,NPV,FNR,FPR,FDR,FOR,TS,STP,ACC,F1, MCC)

  }

  # NA instead of NaN
  if (sum(is.nan(group_metric_matrix))){
    group_metric_matrix[is.nan(group_metric_matrix)] <- NA
  }

  class(group_metric_matrix) <- "group_metric_matrix"
  return(group_metric_matrix)
}
