#' Plot fairness object
#'
#' @description Plot distribution for models output probabilities. See how being in particular subgroup affects models decision.
#'
#' @param x object of class \code{fairness_object}
#' @param ... other plot parameters
#'
#' @import ggplot2
#'
#' @return \code{ggplot2} object
#' @export
#' @rdname plot_density
#'
#' @examples
#'
#' data("compas")
#'
#' glm_compas <- glm(Two_yr_Recidivism ~ ., data = compas, family = binomial(link = "logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism) - 1
#'
#' explainer_glm <- DALEX::explain(glm_compas, data = compas, y = y_numeric)
#'
#' fobject <- fairness_check(explainer_glm,
#'   protected = compas$Ethnicity,
#'   privileged = "Caucasian"
#' )
#'
#' plot_density(fobject)
plot_density <- function(x, ...) {
  stopifnot(class(x) == "fairness_object" | class(x) == "fairness_regression_object")

  explainers <- x$explainers
  m <- length(unique(as.character(x$protected)))
  density_data <- data.frame()

  for (i in seq_along(explainers)) {
    tmp_data <- data.frame(
      probability = explainers[[i]]$y_hat,
      label = rep(x$label[i], length(x$protected)),
      protected = x$protected
    )

    # bind with rest
    density_data <- rbind(density_data, tmp_data)
  }

  limits <- c(0, 1)
  if (inherits(x, "fairness_regression_object")) limits <- NULL

  probability <- protected <- label <- NULL
  p <- ggplot(density_data, aes(probability, protected)) +
    geom_violin(color = "#ceced9", fill = "#ceced9", alpha = 0.5) +
    geom_boxplot(aes(fill = protected), width = 0.3, alpha = 0.5, outlier.alpha = 0) +
    scale_x_continuous(limits = limits) +
    DALEX::theme_drwhy_vertical() +
    scale_fill_manual(values = colors_fairmodels(m)) +
    theme(
      legend.position = "none", # legend off
      strip.placement = "outside",
      strip.text.y = element_text(hjust = 0.5, vjust = 1),
    ) +
    ylab("protected variable") +
    ggtitle("Density plot") +
    facet_grid(rows = vars(label))

  if (inherits(x, "fairness_regression_object")) {
    p <- p + xlab("predicted values")
  }

  p
}
