#' Plot fairness PCA
#'
#' @param x fairness_pca object
#' @param ... other parameters
#'
#'
#' @import ggplot2
#' @import DALEX
#'
#'
#' @return
#' @export
#' @rdname plot_fairness_pca

plot.fairness_pca <- function(x, ...){

  pc_1_2 <- x$pc_1_2
  pca_data <- x$pca_data
  print(pc_1_2)
  lab_x <- paste("PC1: explained", pc_1_2[1]*100,"% of variance" )
  lab_y <- paste("PC2: explained", pc_1_2[2]*100,"% of variance" )


  # to add knn ?

  ggplot(pca_data, aes(PC1, PC2, color = labels)) +
                  geom_point(size = 2) +
                  theme_drwhy() +
                  xlab(lab_x) +
                  ylab(lab_y) +
                  ggtitle("Fairness Metric PCA plot")

}
