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
  lab_x <- paste("PC1: explained", pc_1_2[1]*100,"% of variance" )
  lab_y <- paste("PC2: explained", pc_1_2[2]*100,"% of variance" )




  # to add knn ?

  ggplot(pca_data, aes(PC1, PC2, color = labels, label = labels)) +
                  geom_point(size = 2) +
                  theme_drwhy() +
                  xlab(lab_x) +
                  ylab(lab_y) +
                  ggtitle("Fairness Metric PCA plot") +
      geom_segment(aes(x=rep(0,nrow(pca_data)), y =rep(0,nrow(pca_data)),
                       xend = PC1, yend = PC2 ), arrow = arrow(length = unit(0.01, "cm"))) +
      geom_text(hjust = 0, nudge_x = 0.05, color = "black", check_overlap = TRUE)

}
