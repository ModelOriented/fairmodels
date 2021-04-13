#' Plot fairness PCA
#'
#' @description Plot pca calculated on fairness_object metrics. Similar models and metrics should be close to each other. Plot doesn't work on multiple \code{fairness_pca} objects.
#' Unlike in other plots here other \code{fairness_pca} objects cannot be added.
#'
#'
#' @param x \code{fairness_pca} object
#' @param scale scaling loadings plot, from 0 to 1
#' @param ... other plot parameters
#'
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom DALEX theme_drwhy
#'
#' @return \code{ggplot2} object
#' @export
#' @rdname plot_fairness_pca
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
#' plot(fpca)
#'

plot.fairness_pca <- function(x, scale = 0.5,  ...){


  # scaling like in stats::biplot
  lam <- x$sdev[1:2]
  n   <- nrow(x$x)
  lam <- lam * sqrt(n)
  if(scale != 0) lam <- lam^scale else lam <- 1

  pca_df <- t(t(x$x[,1:2])/lam)
  rotation <-  t(t(x$rotation[,1:2])*lam)

  pc_1_2          <- x$pc_1_2
  pca_data        <- as.data.frame(pca_df)
  pca_data$labels <- x$label

  pca_feature        <- as.data.frame(rotation)
  pca_feature$labels <- rownames(rotation)



  lab_x <- paste("PC1: explained", pc_1_2[1]*100,"% of variance" )
  lab_y <- paste("PC2: explained", pc_1_2[2]*100,"% of variance" )


  n <- nrow(rotation)

  PC1 <- PC2 <- NULL
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
    ggtitle("Fairness PCA plot", subtitle = "created with parity loss metrics")

}

