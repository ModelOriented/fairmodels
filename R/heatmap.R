#' Heatmap
#'
#' @param x fairness object
#' @param midpoint point in score scale
#' @param ... other parameters
#'
#' @return ggplot object

#'
#'
#' @examples
#'library(ranger)
#' library(DALEX)
#' df <- fairness::compas[,1:7]
#' # models
#' y_numeric <- as.numeric(df$Two_yr_Recidivism)-1
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive, data = df, probability = TRUE) # Wszystko
#' model_compas_lr <- glm(Two_yr_Recidivism~., data=df, family=binomial(link="logit"))
#' rf_compas_5 <- ranger(Two_yr_Recidivism ~., data = df, probability = TRUE) # Wszystko
#' rf_compas_6 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = df, probability = TRUE) # dziwny model
#' rf_compas_7 <- ranger(Two_yr_Recidivism ~., data = df, probability = TRUE) # Wszystko
#' rf_compas_8 <- ranger(Two_yr_Recidivism ~ Female+Age_Above_FourtyFive+Misdemeanor+ethnicity, data = df, probability = TRUE) # dziwny model
#'
#' # explainers
#' explainer_1 <- explain(rf_compas_1, data = df, y = y_numeric)
#' explainer_4 <- explain(model_compas_lr, data = df, y = y_numeric)
#' explainer_5 <- explain(rf_compas_5, data = df, y = y_numeric)
#' explainer_6 <- explain(rf_compas_6, data = df, y = y_numeric)
#' explainer_7 <- explain(rf_compas_7, data = df, y = y_numeric)
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
#'                                   group  = "ethnicity",
#'                                   base   = "Caucasian")
#'
#' heatmap(fobject)
#' @export
#' @rdname heatmap



heatmap <- function(x, midpoint = NULL, ...)  UseMethod("heatmap")


#' @export
#' @rdname heatmap
heatmap.fairness_object <- function(x, midpoint = NULL,  ... ){


  heatmap_data <- expand_fairness_object(x)

  heatmap_data <- as.data.frame(heatmap_data)
  colnames(heatmap_data) <- c("metric","model","score")
  heatmap_data$metric <- as.factor(heatmap_data$metric)
  heatmap_data$model <- as.factor(heatmap_data$model)
  heatmap_data$score <- round(as.numeric(heatmap_data$score),3)

  heatmap.deafult(heatmap_data, midpoint)
}


#' @export
#' @rdname heatmap

heatmap.deafult <- function(x, midpoint = NULL, ...){

  if (is.null(midpoint)) midpoint <- max(x$score)/2


  ggplot(x, aes(model, metric, fill = score))  +
    geom_tile(colour = "grey50") +
    scale_fill_gradient2(low="#64f211",mid = "#fbff00", high="#ff0000", midpoint = midpoint) +
    theme_drwhy() +
    ggtitle("Heatmap")
}
