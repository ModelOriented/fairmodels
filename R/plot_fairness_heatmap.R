#' Plot Heatmap
#'
#' @description
#' Heatmap shows all metrics across all models while displaying similarity between variables (in form of dendograms).
#' Heatmap is flexible and is always longer than wider for esthetic reasons. It flips axis while number of explainers exceeds
#' number of metrics. NA's in metrics are gray.
#'
#' @param x fairness object
#' @param ... other plot parameters
#' @param midpoint midpoint on gradient scale
#' @param text deafult \code{TRUE} means it shows values on tiles
#' @param title title of the plot
#' @param subtitle subtitle of the plot
#' @param flip_axis logical, whether to change axis with metrics on axis with models
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
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive, data = compas, probability = TRUE)
#' model_compas_lr <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' rf_compas_5 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' rf_compas_6 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = compas, probability = TRUE)
#' rf_compas_7 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' rf_compas_8 <- ranger(Two_yr_Recidivism ~ Sex+Age_Above_FourtyFive+Misdemeanor+Ethnicity, data = compas, probability = TRUE)
#'
#' # explainers
#' explainer_1 <- explain(rf_compas_1, data = compas, y = y_numeric)
#' explainer_4 <- explain(model_compas_lr, data = compas, y = y_numeric)
#' explainer_5 <- explain(rf_compas_5, data = compas, y = y_numeric)
#' explainer_6 <- explain(rf_compas_6, data = compas, y = y_numeric)
#' explainer_7 <- explain(rf_compas_7, data = compas, y = y_numeric)
#'
#' # different labels
#' explainer_4$label <- "glm"
#' explainer_5$label <- "rf5"
#' explainer_6$label <- "rf6"
#' explainer_7$label <- "rf7"
#'
#'
#' fobject <- create_fairness_object(explainer_1, explainer_4, explainer_5, explainer_6, explainer_7,
#'                                   outcome = "Two_yr_Recidivism",
#'                                   group  = "Ethnicity",
#'                                   base   = "Caucasian",
#'                                   cutoff = 0.5)
#'
#' fheatmap <- fairness_heatmap(fobject)
#' plot(fheatmap)
#'
#' @export
#' @rdname plot_fairness_heatmap


plot.fairness_heatmap <- function(x, ..., midpoint = NULL, title = NULL, subtitle = NULL,   text = TRUE, flip_axis = FALSE){

    list_of_objects <- get_objects(list(x, ...), "fairness_heatmap")
    heatmap_data    <- extract_data(list_of_objects, "heatmap_data")
    matrix_model    <- extract_data(list_of_objects, "matrix_model")

    assert_equal_parameters(list_of_objects, "scale")

    scale        <- x$scale

    if (is.null(midpoint)) midpoint <- max(matrix_model, na.rm = TRUE)/2
    if (scale) midpoint <- 0
    if (!is.numeric(midpoint)) stop("Midpoint must be numeric")
    if (length(midpoint) != 1) stop("Midpoint has lenght > 1")

    # logicals
    stopifnot(is.logical(scale))
    stopifnot(is.logical(text))

    # title and subtitle
    if (is.null(title))    title    <- "Heatmap"
    if (is.null(subtitle)) subtitle <- "With dendograms"
    stopifnot(is.character(title))
    stopifnot(is.character(subtitle))


    # Dendograms -----------------------------------------

    # making top dendogram
    model1 <- hclust(dist(matrix_model))
    dhc1   <- as.dendrogram(model1)


    # dendogram for models
    dendro_data1    <-  dendro_data(dhc1, type = "rectangle")
    dendogram_model <-  ggplot(segment(dendro_data1)) +
                          geom_segment(aes(x = x,
                                           y = y,
                                           xend = xend,
                                           yend = yend),

                                       color = "#371ea3") +
                          # theme = nothing
                          theme_drwhy() +
                          theme(panel.grid= element_blank(),
                                axis.text = element_blank(),
                                axis.title = element_blank())



    # dendogram for metrics
    model2 <- hclust(dist(t(as.matrix(matrix_model))))
    dhc2   <- as.dendrogram(model2)

    dendro_data2     <- dendro_data(dhc2, type = "rectangle")
    dendogram_metric <- ggplot(segment(dendro_data2)) +
                          geom_segment(aes(x = x,
                                           y = y,
                                           xend = xend,
                                           yend = yend),

                                        color = "#371ea3") +
                          # theme = nothing
                          theme_drwhy() +
                          theme(panel.grid= element_blank(),
                                axis.text = element_blank(),
                                axis.title = element_blank())


  # Heatmap ----------------------------------------

  # ordering factors to fit dendogram branches
  model_levels  <- unlist(dendro_data1$labels['label'])
  metric_levels <- unlist(dendro_data2$labels['label'])

  names(model_levels) <- NULL
  names(metric_levels) <- NULL

  # releveling
  heatmap_data$model  <- factor(heatmap_data$model,  levels = model_levels)
  heatmap_data$metric <- factor(heatmap_data$metric, levels = metric_levels)
  heatmap_data$score  <- as.numeric(heatmap_data$score)

  # heatmap
    ifelse(!flip_axis,
           p <- ggplot(heatmap_data, aes(metric, model, fill = score)),
           p <- ggplot(heatmap_data, aes(model, metric, fill = score)))

    heatmap <- p +    geom_tile(colour = "white",
                                size = 0.2,
                                na.rm = TRUE) +
                      scale_fill_gradient2(low="#c7f5bf",
                                           mid = "#46bac2",
                                           high="#371ea3",
                                           midpoint = midpoint,
                                           na.value = 'grey') +
                      theme_drwhy() +
                      theme(legend.position = "bottom",
                            axis.ticks = element_blank(),
                            axis.text.x = element_text(angle = 90)
                            )


  # if text true add text
  if (text) heatmap <- heatmap + geom_text(aes(label = score),
                                           color = "white",
                                           size = 3)

  ifelse(!flip_axis,
         dendogram_right <- dendogram_model  + coord_flip(),
         dendogram_right <- dendogram_metric + coord_flip()
         )

  ifelse(!flip_axis,
         dendogram_top <- dendogram_metric,
         dendogram_top <- dendogram_model
  )

  # adding title
  dendogram_top <-   dendogram_top +
                        ggtitle(title,
                                subtitle = subtitle)

  # Plot layout
  dendogram_top + plot_spacer() +
  heatmap + dendogram_right  +
  plot_layout(ncol = 2,
              widths = c(1,0.4),
              heights = c(0.4,1))
  }
