#' Plot group metric
#'
#' @description Plot chosen metric in group. Notice how models are treating different subgroups. Compare models both in fairness metrics and in performance.
#'
#' @param x object of class group_metric
#' @param ... other group_metric objects and other parameters
#'
#' @return list of \code{ggplot} object
#' @export
#'
#' @rdname plot_group_metric
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#' data("compas")
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#'
#' rf_1 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
#' lr_1 <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#'
#' explainer_1 <- explain(rf_1, data = compas, y = y_numeric)
#' explainer_2 <- explain(lr_1, data = compas, y = y_numeric)
#'
#' fo <- create_fairness_object(explainer_1, explainer_2,  outcome = "Two_yr_Recidivism", group = "Ethnicity", base = "Caucasian"  )
#'
#' gm <- group_metric(fo, fairness_metric = "FPR", performance_metric = "auc")
#' plot(gm)
#'
#'


plot.group_metric <- function(x, ...){

  list_of_objects    <- get_objects(list(x, ...), "group_metric")
  fairness_data      <- extract_data(list_of_objects, "fairness_data")
  performance_data   <- extract_data(list_of_objects, "performance_data")

  assert_equal_parameters(list_of_objects, "performance_metric")
  assert_equal_parameters(list_of_objects, "y_label")

  y_label            <- x$y_label
  performance_metric <- x$performance_metric

  # extracting number of fairness_labels
  n <- length(unique(fairness_data$label))

  plot1 <- ggplot(fairness_data, aes(group, value, fill = label)) +
    geom_bar(stat="identity",
             position = "dodge")  +
    theme_drwhy() +
    theme(axis.text.x=element_text(angle=90, hjust=1),
          legend.position = "none") +
    ylab(y_label) +
    xlab("Models metrics in subgroups") +
    scale_fill_manual(values = DALEX::colors_discrete_drwhy(n = n)) +
    ggtitle("Group metric plot")

  plot2 <- ggplot(performance_data, aes(x,y, fill = x)) +
    geom_bar(stat = "identity",
             width = 0.4) +
    geom_text(aes(label=round(y,3)), vjust=-1, color="black", size=3, fontface = "bold") +
    theme_drwhy() +
    theme(legend.title = element_blank(),
          axis.text.x=element_text(angle=90, hjust=0.3)) +
    scale_fill_manual(values = DALEX::colors_discrete_drwhy(n = n)) +
    scale_y_continuous(limits = c(0,1))+
    xlab("Models") +
    ylab(performance_metric)

  plot1 + plot2
}

