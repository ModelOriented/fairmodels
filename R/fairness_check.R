#' Fairness check
#'
#' @description Method for quick popular fairness metrics check.
#'
#' @param x fairness_object
#'
#' @param epsilon numeric, margins for decision if fair or not. Default 0.1
#'
#' @details Metrics used are made for each subgroup, then base metric score is subtracted leaving loss of particular metric.
#' If loss is less than -epsilon than such metric is marked as "not passed". There is no upper boundary for fairness, so if some subgroup is "better"
#' in particular metric than privileged one there are no consequences to that. Good metric description can be found here :
#' If loss is greater than absolute value of epsilon than such metric is marked as "not passed". It means that values of metrics should be within (-epsilon,epsilon) boundary.
#' Epsilon value can be adjusted to user's needs. There are some metrics that might be derived from existing metrics (For example Equalized Odds - equal TPR and FPR for all subgroups).
#' That means passing 5 metrics in fairness check asserts that model is even more fair. In \code{fairness_check} models must always predict commonly desired result. Not adhering to this rule
#' may lead to misinterpretation of the plot. More on metrics and their equivalents:
#' \url{https://fairware.cs.umass.edu/papers/Verma.pdf}
#' \url{https://en.wikipedia.org/wiki/Fairness_(machine_learning)}
#'
#'
#'
#' @return
#' @export
#'
#' @references
#' Zafar,Valera, Rodriguez, Gummadi (2017)  \url{https://arxiv.org/pdf/1610.08452.pdf}
#'
#' Hardt, Price, Srebro (2016) \url{https://arxiv.org/pdf/1610.02413.pdf}
#'
#' Verma, Rubin (2018) \url{https://fairware.cs.umass.edu/papers/Verma.pdf}
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#'
#' # chaging levels - positive situation here is not becoming recidivist
#' compas$Two_yr_Recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
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
#'                                  outcome = "Two_yr_Recidivism",
#'                                  data  = compas,
#'                                  group = "Sex",
#'                                  base  = "Female")
#'
#' # changing cutoff values
#' fobject2 <-create_fairness_object(explainer_glm, explainer_rf,
#'                                  outcome = "Two_yr_Recidivism",
#'                                  data  = compas,
#'                                  group = "Sex",
#'                                  base  = "Female",
#'                                 cutoff = c(0.45,0.5),
#'                                 fairness_labels = c("lm_c","rf_c")) # c for changed
#'
#' fc <- fairness_check(fobject)
#' fc2 <- fairness_check(fobject2)
#'
#' plot(fc,fc2)
#'

fairness_check <- function(x, epsilon = 0.1){

  stopifnot(class(x) == "fairness_object")

  if(! is.numeric(epsilon) | length(epsilon) > 1) stop("Epsilon must be singlenumeric value")

  base   <- x$base
  labels <- x$labels
  explainers <- x$explainers

  df <- data.frame()

  for (i in seq_along(explainers)){

  explainer <- explainers[[i]]

  model  <- x$fairness_labels[i]
  # get based metrics from all
  parity_loss_metrics <- lapply(x$groups_data[[model]], function(y) y - y[base])

  # omit base metric because it is always 0
  parity_loss_metrics <- lapply(parity_loss_metrics, function(x) x[names(x) != base])

  exp_data <- x$data
  exp_data$`_probabilities_` <- explainer$y_hat

  gm        <- group_matrices(exp_data,
                              x$group,
                              outcome = x$outcome,
                              outcome_numeric = explainer$y,
                              cutoff = x$cutoff)

  # statistical parity - positive predictions / all
  statistical_parity <-  lapply(gm, function(x) (x$tp + x$fp) /(x$tp + x$fp + x$tn + x$fn))
  statistical_parity_difference <- lapply(statistical_parity, function(x) x - statistical_parity[base][[1]])
  statistical_parity_difference <- statistical_parity_difference[names(statistical_parity_difference) != base]

  # assigning metrics
  equal_oportunity_loss     <- parity_loss_metrics$TPR
  predictive_parity_loss    <- parity_loss_metrics$PPV
  predictive_equality_loss  <- parity_loss_metrics$FPR
  accuracy_equality_loss    <- parity_loss_metrics$ACC

  n_sub <- length(levels(x$data[,x$group])) -1
  n_exp <- length(x$explainers)

  # creating data frames
  statistical_parity_data <- data.frame(score    = unlist(statistical_parity_difference),
                                        subgroup = names(statistical_parity_difference),
                                        metric   = rep("Statistical parity loss  (TP + FP)/(TP + FP + TN + FN)", n_sub),
                                        model    = rep(model, n_sub))

  predictive_parity_data  <- data.frame(score    = unlist(predictive_parity_loss),
                                        subgroup = names(predictive_parity_loss),
                                        metric   = rep("Predictive parity loss    TP/(TP + FP)", n_sub),
                                        model    = rep(model, n_sub))

  equal_opportunity_data  <- data.frame(score    = unlist(equal_oportunity_loss),
                                        subgroup = names(equal_oportunity_loss),
                                        metric   = rep("Equal opportynity loss    TP/(TP + FN) ", n_sub),
                                        model    = rep(model, n_sub))

  predictive_equality_data<- data.frame(score    = unlist(predictive_equality_loss),
                                        subgroup = names(predictive_equality_loss),
                                        metric   = rep("Predictive equality loss  FP/(FP + TN)", n_sub),
                                        model    = rep(model, n_sub))

  accuracy_equality_data  <- data.frame(score    = unlist(accuracy_equality_loss),
                                        subgroup = names(accuracy_equality_loss),
                                        metric   = rep("Accuracy equality loss   (TP + FN)/(TP + FP + TN + FN) ", n_sub),
                                        model    = rep(model, n_sub))

  # add metrics to dataframe
  df <- rbind(df,
              equal_opportunity_data,
              predictive_parity_data,
              predictive_equality_data,
              accuracy_equality_data,
              statistical_parity_data)

  }

  rownames(df) <- NULL

  fairness_check <- list(data = df, n_exp = n_exp, n_sub = n_sub, epsilon = epsilon,fairness_labels = x$fairness_labels)
  class(fairness_check) <- "fairness_check"

  return(fairness_check)
}






?expression

expression(Value~is~sigma~R^{2}==0.6)


