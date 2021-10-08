#' Plot Heatmap
#'
#' @description
#' Heatmap shows all parity loss metrics across all models while displaying similarity between variables (in form of dendograms). All metrics are visible. Some have identical values
#' as it should be in terms of their parity loss (eg. TPR parity loss == FNR parity loss, because TPR = 1 - FNR ).
#' NA's in metrics are gray.
#'
#' @param x \code{fairness_heatmap}
#' @param ... other \code{fairness_heatmap} objects
#' @param midpoint numeric, midpoint on gradient scale
#' @param text logical, default \code{TRUE} means it shows values on tiles
#' @param title character, title of the plot
#' @param subtitle character, subtitle of the plot
#' @param flip_axis logical, whether to change axis with metrics on axis with models
#' @param text_size numeric, size of text
#'
#' @return list of \code{ggplot2} objects
#'
#' @import patchwork
#' @import ggplot2
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) - 1
#'
#' lm_model <- glm(Risk ~ .,
#'   data = german,
#'   family = binomial(link = "logit")
#' )
#'
#' rf_model <- ranger::ranger(Risk ~ .,
#'   data = german,
#'   probability = TRUE,
#'   num.trees = 200,
#'   num.threads = 1,
#'   seed = 1
#' )
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[, -1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'   protected = german$Sex,
#'   privileged = "male"
#' )
#'
#' # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'   protected = german$Sex,
#'   privileged = "male",
#'   cutoff = list(female = 0.4),
#'   label = c("lm_2", "rf_2")
#' )
#'
#'
#' fh <- fairness_heatmap(fobject)
#'
#' plot(fh)
#' @export
#' @rdname plot_fairness_heatmap


plot.fairness_heatmap <- function(x, ...,
                                  midpoint = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  text = TRUE,
                                  text_size = 3,
                                  flip_axis = FALSE) {
  if (!requireNamespace("ggdendro", quietly = TRUE)) {
    stop("Package \"ggdendro\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }


  matrix_model <- x$matrix_model
  data <- x$heatmap_data
  scale <- x$scale

  if (is.null(midpoint)) midpoint <- max(matrix_model, na.rm = TRUE) / 2
  if (scale) midpoint <- 0
  if (!is.numeric(midpoint)) stop("Midpoint must be numeric")
  if (length(midpoint) != 1) stop("Midpoint has lenght > 1")

  # logical
  stopifnot(is.logical(scale))
  stopifnot(is.logical(text))

  # title and subtitle
  if (is.null(title)) title <- "Heatmap"
  if (is.null(subtitle)) subtitle <- "With dendograms"
  stopifnot(is.character(title))
  stopifnot(is.character(subtitle))

  # text size
  stopifnot(is.numeric(text_size))
  stopifnot(length(text_size) == 1)
  stopifnot(text_size > 0)

  # prepare variables
  y <- xend <- yend <- parity_loss_metric <- model <- score <- NULL

  # Dendograms -----------------------------------------

  # making top dendogram
  model1 <- stats::hclust(stats::dist(matrix_model))
  dhc1 <- stats::as.dendrogram(model1)


  # dendogram for models
  dendro_data1 <- ggdendro::dendro_data(dhc1, type = "rectangle")
  dendogram_model <- ggplot(ggdendro::segment(dendro_data1)) +
    geom_segment(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    color = "#371ea3"
    ) +
    # theme = nothing
    DALEX::theme_drwhy() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  # dendogram for metrics
  model2 <- stats::hclust(stats::dist(t(as.matrix(matrix_model))))
  dhc2 <- stats::as.dendrogram(model2)

  dendro_data2 <- ggdendro::dendro_data(dhc2, type = "rectangle")
  dendogram_metric <- ggplot(ggdendro::segment(dendro_data2)) +
    geom_segment(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    color = "#371ea3"
    ) +
    # theme = nothing
    DALEX::theme_drwhy() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )

  # Heatmap ----------------------------------------

  # ordering factors to fit dendogram branches
  model_levels <- unlist(dendro_data1$labels["label"])
  metric_levels <- unlist(dendro_data2$labels["label"])

  names(model_levels) <- NULL
  names(metric_levels) <- NULL

  # releveling
  data$model <- factor(data$model, levels = model_levels)
  data$parity_loss_metric <- factor(data$parity_loss_metric, levels = metric_levels)
  data$score <- as.numeric(data$score)

  # heatmap
  ifelse(!flip_axis,
    p <- ggplot(data, aes(parity_loss_metric, model, fill = score)),
    p <- ggplot(data, aes(model, parity_loss_metric, fill = score))
  )

  heatmap <- p + geom_tile(
    colour = "white",
    size = 2,
    na.rm = TRUE
  ) +
    scale_fill_gradient2(
      low = "#c7f5bf",
      mid = "#46bac2",
      high = "#371ea3",
      midpoint = midpoint,
      na.value = "grey"
    ) +
    DALEX::theme_drwhy() +
    theme(
      legend.position = "bottom",
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 90)
    )

  # if text true add text
  if (text) {
    heatmap <- heatmap + geom_text(aes(label = score),
      color = "white",
      size = text_size,
      fontface = "bold"
    )
  }

  ifelse(!flip_axis,
    dendogram_right <- dendogram_model + coord_flip(),
    dendogram_right <- dendogram_metric + coord_flip()
  )

  ifelse(!flip_axis,
    dendogram_top <- dendogram_metric,
    dendogram_top <- dendogram_model
  )

  # adding title
  dendogram_top <- dendogram_top +
    ggtitle(title,
      subtitle = subtitle
    )

  # Plot layout
  dendogram_top + plot_spacer() +
    heatmap + dendogram_right +
    patchwork::plot_layout(
      ncol = 2,
      widths = c(1, 0.4),
      heights = c(0.4, 1)
    )
}
