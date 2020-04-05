#' Plot Heatmap
#'
#' @param x fairness object
#'
#' @return ggplot object
#'
#'
#' @examples
#'
#' library(ranger)
#' library(DALEX)
#' data(compas)
#'
#' # models
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive, data = compas, probability = TRUE) # Wszystko
#' model_compas_lr <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' rf_compas_5 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE) # Wszystko
#' rf_compas_6 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = compas, probability = TRUE) # dziwny model
#' rf_compas_7 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE) # Wszystko
#' rf_compas_8 <- ranger(Two_yr_Recidivism ~ Sex+Age_Above_FourtyFive+Misdemeanor+Ethnicity, data = compas, probability = TRUE) # dziwny model
#'
#' # explainers
#' explainer_1 <- explain(rf_compas_1, data = compas, y = y_numeric)
#' explainer_4 <- explain(model_compas_lr, data = compas, y = y_numeric)
#' explainer_5 <- explain(rf_compas_5, data = compas, y = y_numeric)
#' explainer_6 <- explain(rf_compas_6, data = compas, y = y_numeric)
#' explainer_7 <- explain(rf_compas_7, data = compas, y = y_numeric)
#'
#' # diffrent labels
#' explainer_4$label <- "glm"
#' explainer_5$label <- "rf5"
#' explainer_6$label <- "rf6"
#' explainer_7$label <- "rf7"
#'
#'
#' fobject <- create_fairness_object(explainer_1,explainer_4,explainer_5,explainer_6,explainer_7,
#'                                   outcome = "Two_yr_Recidivism",
#'                                   group  = "Ethnicity",
#'                                   base   = "Caucasian")
#'
#' plot_heatmap(fobject)
#'
#' @export
#' @rdname plot_heatmap


plot_heatmap <- function(x){

    # m <- ncol(x$metric_data)
    #
    # heatmap_data <- expand_fairness_object(x)
    # heatmap_data <- as.data.frame(heatmap_data)
    # colnames(heatmap_data) <- c("metric","model","score")
    # heatmap_data$metric <- as.factor(heatmap_data$metric)
    # heatmap_data$model <- as.factor(heatmap_data$model)
    # heatmap_data$score <- round(as.numeric(heatmap_data$score),3)
    #
    #
    #
    # dist(as.matrix(x$metric_data[,1:(m-1)]))
    # hclust()
    # as.dendrogram()
    # ggdendrogram(, rotate = TRUE)

  ht  <- as.matrix(x$metric_data[,1:(ncol(x$metric_data)-1)])
  rownames(ht) <- unlist(x$metric_data["model labels"])

  ht_transposed <- t(ht)
  rownames(ht_transposed) <- colnames(x$metric_data)[1:(ncol(x$metric_data)-1)]

  heat_map <- stats::heatmap(ht_transposed)
  heat_map
    # if (is.null(midpoint)) midpoint <- max(heatmap_data$score)/2
    #
    #
    # ggplot(heatmap_data, aes(model, metric, fill = score))  +
    #   geom_tile(colour = "grey50") +
    #   scale_fill_gradient2(low="#64f211",mid = "#fbff00", high="#ff0000", midpoint = midpoint) +
    #   theme_drwhy() +
    #   ggtitle("Heatmap")
}
