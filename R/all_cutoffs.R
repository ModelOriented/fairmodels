#' All cutoffs
#'
#' @description Create all_cutoffs object and to show how with change of cutoffs parity metrics change for single model/explainer.
#' It is highly suggested to pick small number of metrics.
#'
#' @param x fairness_object
#' @param grid_points numeric, grid for cutoffs to test. Number of points between 0 and 1 spread evenly.
#' @param fairness_metrics character, name of metric or vector of metrics
#' @param explainer_label character, single label of choosen explainer
#'
#' @return all_cutoffs object
#' @export
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
#'                                  outcome = "Two_yr_Recidivism",
#'                                  group = "Ethnicity",
#'                                  base = "Caucasian",
#'                                  cutoff = 0.5)
#'
#' ac <- all_cutoffs(fobject, fairness_metrics = c("TPR_parity_loss", "F1_parity_loss"), explainer_label = "ranger")
#' plot(ac)
#'


all_cutoffs <- function(x, grid_points = 101, fairness_metrics = unique_metrics(), explainer_label){

  stopifnot(class(x) == "fairness_object")

  # error if not in metrics
  lapply(fairness_metrics, assert_parity_metrics)

  if (! is.numeric(grid_points) | length(grid_points) > 1) stop("grid points must be single numeric value")
  if (! is.character(explainer_label) | length(explainer_label) > 1)  stop("explainer_label must be character")

  labels <- sapply(x$explainers, function(x) x$label)
  if (! explainer_label %in% labels ) stop ("explainer_label not in provided labels")

  explainers <- x$explainers
  cutoffs    <- seq(0,1, length.out =  grid_points)
  data       <- x$data
  group      <- x$group
  outcome    <- x$outcome
  base       <- x$base

  n_subgroups <- length(x$cutoff)
  cutoff_data <- data.frame()

  # custom cutoffs will give messages (0 in matrices, NA in metrics)  numerous times,
  # so for code below they will be suppressed

  suppressMessages(
  for (custom_cutoff in cutoffs){

      custom_cutoff_vec  <- rep(custom_cutoff, n_subgroups)
      explainer          <- explainers[sapply(explainers , function(x) x$label) == explainer_label][[1]]
      data$probabilities <- explainer$y_hat
      label              <- explainer$label

      group_matrices <- group_matrices(data,
                                       group = group,
                                       outcome = outcome,
                                       outcome_numeric = explainer$y,
                                       cutoff = custom_cutoff_vec)

      # like in create fobject
      gmm             <- calculate_group_fairness_metrics(group_matrices)
      gmm_scaled      <- abs(apply(gmm, 2 , function(x) x  - gmm[,base]))
      gmm_loss        <- rowSums(gmm_scaled)
      names(gmm_loss) <- paste0(names(gmm_loss),"_parity_loss")

      gmm_loss_unique <- gmm_loss[names(gmm_loss) %in% fairness_metrics]

      to_add <- data.frame(parity_loss = as.numeric(gmm_loss_unique),
                           metric = names(gmm_loss_unique),
                           cutoff = rep(custom_cutoff, length(gmm_loss_unique)))

      cutoff_data <- rbind(cutoff_data , to_add)

    })

  all_cutoffs <- list(data = cutoff_data, explainer_label = explainer_label)
  class(all_cutoffs) <- "all_cutoffs"

  return(all_cutoffs)
}















