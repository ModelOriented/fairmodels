#' Fairness check
#'
#' @description Method for quick popular fairness metrics check. On contrary to parity loss metrics here fairness_check bases on group_metrics
#' and worse performance in particular metric means, that subgroup scored worse than privlieged (base) subgroup. Note that being worse in particular
#' metric depends on assumption that models try to predict positive outcome. Therefore Equal odds here calculates True positive rate. Unlike in Parity
#' loss metrics assigning subjectively negative metric as positive (numeric 1) will end up in method's wrong interpretation of outcome. When plotted
#' shows 5 metric scores for each subgroups and each model.
#'
#' @param x fairness_object
#'
#' @param epsilon numeric, margins for decision if fair or not. Deafult 0.1
#'
#' @details Metrics used are made for each subgroup, then base metric score is subtracted leaving loss of particular metric.
#' If loss is less than -epsilon than such metric is marked as "not passed". There is no upper boundry for fairness, so if some subgroup is "better"
#' in particular metric than privliedged one there are no consequences to that. Good metric description can be found here :
#' \url{https://en.wikipedia.org/wiki/Fairness_(machine_learning)}
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
#'
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
#'                                  base  = "Female",
#'                                 cutoff = 0.5)
#'
#' fc <- fairness_check(fobject)
#' plot(fc)
#'

fairness_check <- function(x, epsilon = 0.1){

  stopifnot(class(x) == "fairness_object")

  if(! is.numeric(epsilon) | length(epsilon) > 1) stop("Epsilon must be singlenumeric value")

  base  <- x$base

  explainer_labels <- sapply(x$explainers , function(x) x$label)


  df<- data.frame()


  for (model in explainer_labels){
  # get based metrics from all
  parity_loss_metrics <- lapply(x$groups_data[model][[1]], function(y) y - y[base])

  # omit base metric because it is always 0
  parity_loss_metrics <- lapply(parity_loss_metrics, function(x) x[names(x) != base])

  # calculating new metrics
  explainer <- x$explainers[sapply(x$explainers , function(x) x$label) == model][[1]]
  gm        <- group_matrices(x$data,
                              x$group,
                              outcome = x$outcome,
                              outcome_numeric = explainer$y,
                              cutoff = x$cutoff)

  statistical_parity <-  lapply(gm, function(x) (x$tp + x$fp) /(x$tp + x$fp + x$tn + x$fn))

  statistical_parity_difference <- lapply(statistical_parity, function(x) x - statistical_parity[base][[1]])
  statistical_parity_difference <- statistical_parity_difference[names(statistical_parity_difference) != base]

  # assigning metrics
  equal_odds_loss        <- parity_loss_metrics$TPR
  equal_oportunity_loss  <- parity_loss_metrics$PPV
  NPV_loss               <- parity_loss_metrics$NPV
  FPR_loss               <- parity_loss_metrics$FPR
  FNR_loss               <- parity_loss_metrics$FNR

  n_sub <- length(levels(x$data[,x$group])) -1
  n_exp <- length(explainer_labels)

  # creating data frames
  statistical_parity_data <- data.frame(score    = unlist(statistical_parity_difference),
                                        subgroup = names(statistical_parity_difference),
                                        metric   = rep("Statistical parity loss", n_sub),
                                        model    = rep(model, n_sub))

  equal_odds_data         <- data.frame(score    = unlist(equal_odds_loss),
                                        subgroup = names(equal_odds_loss),
                                        metric   = rep("Equal odds loss", n_sub),
                                        model    = rep(model, n_sub))

  # inverse to match others in terms of interpretation
  inversed_equal_opportunity_data  <- data.frame(score = -unlist(equal_oportunity_loss),
                                        subgroup = names(equal_oportunity_loss),
                                        metric   = rep("Inversed Equal opportynity loss", n_sub),
                                        model    = rep(model, n_sub))

  NPV_data  <- data.frame(score = unlist(NPV_loss),
                          subgroup = names(NPV_loss),
                          metric   = rep("Negative predictive value loss", n_sub),
                          model    = rep(model, n_sub))

  FPR_data  <- data.frame(score = unlist(FPR_loss),
                          subgroup = names(FPR_loss),
                          metric   = rep("False positive loss", n_sub),
                          model    = rep(model, n_sub))

  FNR_data  <- data.frame(score = unlist(FNR_loss),
                          subgroup = names(FNR_loss),
                          metric   = rep("False negative loss", n_sub),
                          model    = rep(model, n_sub))


  # add metrics to dataframe
  df <- rbind(df,
                statistical_parity_data,
                inversed_equal_opportunity_data,
                equal_odds_data,
                FPR_data,
                NPV_data)

  }

  rownames(df) <- NULL

  fairness_check <- list(data = df, n_exp = n_exp, n_sub = n_sub, epsilon = epsilon)
  class(fairness_check) <- "fairness_check"

  return(fairness_check)
}











