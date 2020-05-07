#' Demographic parity
#'
#' @description Demographic parity is popular choice to visualize parity on dataset level. In perfect dataset classes should be evenly distributed across subgroups. Plot allows
#' user to see not only how classes are distributed but also the size of subgroups. Plot allows to localize problems in dataset level. Function can recieve data frame or fairness object.
#'
#' @param x data.frame/ fairness_object
#' @param ... other parameters
#' @param outcome target column name in data
#' @param group protected group (column) in data
#'
#'
#' @import ggplot2
#' @return
#'
#' @rdname demographic_parity
#' @export
#'
#' @examples
#'
#' data("compas")
#' demographic_parity(compas, outcome =  "Two_yr_Recidivism", group = "Ethnicity")
#'

demographic_parity <- function(x,...) UseMethod("demographic_parity")



#' @rdname demographic_parity
#' @export

demographic_parity.fairness_object <- function(x, ...){
  data    <- x$data
  outcome <- x$outcome
  group   <- x$group

  demographic_parity.default(x = data, outcome = outcome, group = group)
}

#' @rdname demographic_parity
#' @export

demographic_parity.default <- function(x,..., outcome, group){

  data <-x

  if (! outcome %in% colnames(data)) stop(cat("\nColumn", outcome, "not in data.frame\n"))
  if (! group %in% colnames(data)) stop(cat("\nColumn", group, "not in data.frame\n"))

  if(length(levels(data[,outcome])) != 2) stop("Outcome column must have exacly 2 levels")

  df <- data[, c(outcome, group)]
  df$first_fill <- rep("first",nrow(df) )

  colnames(df) <- c("outcome", "group", "first_fill")

  sorted_lvls <-  names(sort(table(df$group),decreasing = TRUE))

  df$group      <- factor(df$group, levels = c(sorted_lvls))
  df$first_fill <- factor(df$first_fill, levels = c(levels(df$outcome), "first"))


  ggplot(data = df) +
    geom_bar(aes(group), stat = "count", alpha = 0.5, fill = "#ceced9") +
    geom_bar(aes(group, fill = outcome), stat = "count", position = "dodge") +
    scale_fill_manual(name ="observation type", label = c(levels(df$outcome),"all observations" ), values = c(DALEX::colors_discrete_drwhy(2))) +
    theme_drwhy() +
    xlab(group) +
    ylab("Number of observations") +
    ggtitle("Demographic parity plot")

}


















