

#' Plot fairness PCA
#'
#' @description Plot pca calculated on fainress_object
#'
#'
#' @param x fairness_pca object
#' @param scale scaling loadings plot, from 0 to 1
#' @param ... other parameters
#'
#'
#' @import ggplot2
#' @import DALEX
#' @import ggrepel
#'
#' @return
#' @export
#' @rdname plot_fairness_pca
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism) - 1
#' # models
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~ Number_of_Priors + Age_Below_TwentyFive, data = compas, probability = TRUE)
#' rf_compas_3 <- ranger(Two_yr_Recidivism ~ Sex + Age_Above_FourtyFive, data = compas, probability = TRUE)
#' model_compas_lr <- glm(Two_yr_Recidivism ~ ., data = compas, family = binomial(link = "logit"))
#' rf_compas_5 <- ranger(Two_yr_Recidivism ~ ., data = compas, probability = TRUE)
#' rf_compas_6 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive + Misdemeanor, data = compas, probability = TRUE)
#' rf_compas_7 <- ranger(Two_yr_Recidivism ~ ., data = compas, probability = TRUE)
#' rf_compas_8 <- ranger(Two_yr_Recidivism ~ Sex + Age_Above_FourtyFive + Misdemeanor + Ethnicity, data = compas, probability = TRUE)
#'
#' # explainers
#' explainer_1 <- explain(rf_compas_1, data = compas, y = y_numeric)
#' explainer_3 <- explain(rf_compas_3, data = compas, y = y_numeric)
#' explainer_4 <- explain(model_compas_lr, data = compas, y = y_numeric)
#' explainer_5 <- explain(rf_compas_5, data = compas, y = y_numeric)
#' explainer_6 <- explain(rf_compas_6, data = compas, y = y_numeric)
#' explainer_7 <- explain(rf_compas_7, data = compas, y = y_numeric)
#'
#' # different labels
#' explainer_3$label <- "rf3"
#' explainer_4$label <- "glm"
#' explainer_5$label <- "rf5"
#' explainer_6$label <- "rf6"
#' explainer_7$label <- "rf7"
#' explainers <- list(explainer_1, explainer_3, explainer_4, explainer_5, explainer_6, explainer_7)
#'
#' fobject <- create_fairness_object(explainer_1, explainer_3, explainer_4, explainer_5, explainer_6, explainer_7,
#'   outcome = "Two_yr_Recidivism",
#'   group = "Ethnicity",
#'   base = "Caucasian"
#' )
#'
#' fpca <- pca(fobject)
#' plot(fpca)

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

