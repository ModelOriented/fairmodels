#' Plot Heatmap
#'
#' @param x fairness object
#' @param scale whether metrics should be normalised
#' @param midpoint midpoint on gradient scale
#' @param text deafult \code{TRUE} means it shows values on tiles
#' @param title title of the plot
#' @param subtitle subtitle of the plot
#'
#' @return ggplot object
#'
#' @import patchwork
#' @import ggplot2
#' @import ggdendro
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


plot_heatmap <- function(x, midpoint = NULL, title = NULL, subtitle = NULL,   text = TRUE, scale = FALSE){

    m <- ncol(x$metric_data)

    heatmap_data <- expand_fairness_object(x, scale = scale)
    heatmap_data <- as.data.frame(heatmap_data)
    colnames(heatmap_data) <- c("metric","model","score")
    heatmap_data$metric <- as.factor(heatmap_data$metric)
    heatmap_data$model <- as.factor(heatmap_data$model)
    heatmap_data$score <- round(as.numeric(heatmap_data$score),3)

    # getting numerical data and if scale, normalising it
    matrix_model <- as.matrix(x$metric_data[,1:(m-1)])
    if (scale) matrix_model <- scale(matrix_model)

    rownames(matrix_model) <- x$metric_data[,m]

    # making top dendogram
    model1 <- hclust(dist(matrix_model))
    dhc1 <- as.dendrogram(model1)

    # title and subtitle
    if (is.null(title))    title    <- "Heatmap"
    if (is.null(subtitle)) subtitle <- "With dendograms"

    dendro_data1 <- dendro_data(dhc1, type = "rectangle")
    dendogram_top <-   ggplot(segment(dendro_data1)) +
                        geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
                        # theme = nothing
                        theme_drwhy() +
                        theme(panel.grid= element_blank(),
                              axis.text = element_blank(),
                              axis.title = element_blank()) +
                        ggtitle(title, subtitle = subtitle)


    # right dendogram
    model2 <- hclust(dist(t(as.matrix(matrix_model))))
    dhc2 <- as.dendrogram(model2)

    dendro_data2 <- dendro_data(dhc2, type = "rectangle")
    dendogram_right <-   ggplot(segment(dendro_data2)) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
      # theme = nothing
      theme_minimal() +
      theme(panel.grid= element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank()) +
      coord_flip()




  if (is.null(midpoint)) midpoint <- max(matrix_model)/2
  if (scale) midpoint <- 0
  # ordering factors to fit dendogram branches
  model_levels <- levels(unlist(dendro_data1$labels['label']))
  metric_levels <- levels(unlist(dendro_data2$labels['label']))

  # releveling
  heatmap_data$model  <- factor(heatmap_data$model,  levels = model_levels)
  heatmap_data$metric <- factor(heatmap_data$metric, levels = metric_levels)

  # heatmap
  heatmap <-   ggplot(heatmap_data, aes(model, metric, fill = score))  +
                      geom_tile(colour = "grey50") +
    geom_text(aes(label = score), color = "white") +
                      scale_fill_gradient2(low="navyblue",
                                           mid = "darkmagenta",
                                           high="darkorange1",
                                           midpoint = midpoint) +
                      theme_drwhy() +
                      theme(legend.position = "bottom",
                            axis.ticks = element_blank()
                            )

  # if text true add text
  if (text) heatmap <- heatmap + geom_text(aes(label = score), color = "white")

  # Plot layout
  dendogram_top + plot_spacer() +
  heatmap + dendogram_right  +
  plot_layout(ncol = 2,
              widths = c(1,0.4),
              heights = c(0.4,1))
  }
