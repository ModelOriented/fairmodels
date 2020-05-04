#' Plot group metric
#'
#' @description Plot choosen metric in group. Notice how models are treatung different subgroups. Compare models both in fairness metrics and in performance.
#'
#' @param x object of class group_metric
#' @param ... other params
#'
#' @return list of \code{ggplot} object
#' @export
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


plot.group_metric <- function(x, ...){

  fairness_data      <- x$fairness_data
  performance_data   <- x$performance_data
  y_label            <- x$y_label
  performance_metric <- x$performance_metric

  plot1 <- ggplot(fairness_data, aes(group, value, fill = label)) +
    geom_bar(stat="identity",
             position = "dodge")  +
    theme_drwhy() +
    theme(axis.text.x=element_text(angle=90, hjust=1),
          legend.position = "none") +
    ylab(y_label) +
    xlab("Models metrics in groups") +
    scale_fill_manual(values=c("#8bdcbe", "#4378bf")) +
    ggtitle("Group metric plot")

  plot2 <- ggplot(performance_data, aes(x,y, fill = x)) +
    geom_bar(stat = "identity",
             width = 0.4) +
    geom_text(aes(label=round(y,3)), vjust=-1, color="black", size=3, fontface = "bold") +
    ylab("Accuracy") +
    theme_drwhy() +
    theme(legend.title = element_blank(),
          axis.text.x=element_text(angle=0, hjust=0.3)) +
    scale_fill_manual(values=c("#8bdcbe", "#4378bf")) +
    scale_y_continuous(limits = c(0,1))+
    xlab("Models") +
    ylab(performance_metric)

  plot1 + plot2
}

