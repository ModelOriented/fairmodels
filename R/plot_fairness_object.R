#' Plot fairness object
#'
#' @param x fairness_object
#' @param ... other parameters
#'
#' @return \code{ggplot} object
#' @export
#' @rdname plot_fairness_object
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
#' explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)
#'
#' fobject <-create_fairness_object(explainer_glm, explainer_rf,
#'                              outcome = "Two_yr_Recidivism",
#'                              group = "Ethnicity",
#'                              base = "Caucasian",
#'                              cutoff = 0.4)
#' plot(fobject)

plot.fairness_object <- function(x, ...){

  explainers <- x$explainers
  data <- x$data
  n <- nrow(data)
  l <- length(explainers)

  data_combined <- data.frame()

  for (i in seq_len(l)){
    data$probability <- explainers[[i]]$y_hat
    data$label <- rep(explainers[[i]]$label, n )

    # bind with rest
    data_combined <- rbind(data_combined , data)
  }

  # rename columns
  to_change <- match(TRUE , colnames(data_combined) == x$group)
  colnames(data_combined)[to_change] <- "group"


  p <- ggplot(data_combined, aes(probability, group)) +
        geom_violin(color = "grey", fill = "grey" , alpha = 0.8) +
        geom_vline(xintercept = x$cutoff, linetype = "dashed") +
        geom_boxplot(aes(fill = group) ,width = 0.3, alpha = 0.5, outlier.alpha = 0.6) +
        scale_x_continuous(limits = c(0,1)) +
        theme_drwhy_vertical() +
        ylab(x$group)
  p + facet_grid(rows = vars(label))

}





