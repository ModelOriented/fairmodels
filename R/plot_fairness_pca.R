#' Plot fairness PCA
#'
#' @param x fairness_pca object
#' @param scale tbd- za mało wiem jak na razie
#' @param ... other parameters
#'
#'
#' @import ggplot2
#' @import DALEX
#' @import ggrepel
#'
#'
#' @return
#' @export
#' @rdname plot_fairness_pca

plot.fairness_pca <- function(x, scale = 0.5,  ...){


  # scaling like in stats::biplot
  lam <- x$sdev[1:2]
  n <- nrow(x$x)
  lam <- lam * sqrt(n)
  if(scale != 0) lam <- lam^scale else lam <- 1

  pca_df <- t(t(x$x[,1:2])/lam)
  rotation <-  t(t(x$rotation[,1:2])*lam)

  pc_1_2 <- x$pc_1_2
  pca_data <- as.data.frame(pca_df)
  pca_data$labels <- x$labels

  pca_feature <- as.data.frame(rotation)
  pca_feature$labels <- rownames(rotation)



  lab_x <- paste("PC1: explained", pc_1_2[1]*100,"% of variance" )
  lab_y <- paste("PC2: explained", pc_1_2[2]*100,"% of variance" )


  n <- nrow(rotation)


  ggplot() +
            # hline covers lines from theme
            geom_hline(yintercept = 0, color = "white",     linetype = "dashed") +
            geom_vline(xintercept = 0, color = "lightgrey", linetype = "dashed" ) +
            geom_segment(data = pca_feature,
                         aes(x=rep(0,n),
                             y =rep(0,n),
                             xend = PC1,
                             yend = PC2 ),
                         color = "red",
                         alpha = 0.5,
                         arrow = arrow(length = unit(0.2, "cm"))) +
            geom_text_repel(data = pca_feature,
                        aes(  x = PC1,
                              y = PC2,
                              label = labels),
                              color = "red", alpha = 0.5, size = 4) +
            geom_text_repel(data = pca_data,
                            aes(PC1, PC2, label = labels),
                            size = 5,
                            color = "black") +
            geom_point(data = pca_data, aes(PC1, PC2)) +
              theme_drwhy() +
              theme(legend.position = "none") + #without legend
              xlab(lab_x) +
              ylab(lab_y) +
              ggtitle("Fairness Metric PCA plot")

}
