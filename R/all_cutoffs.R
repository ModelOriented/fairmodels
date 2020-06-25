#' All cutoffs
#'
#' @description Create all_cutoffs object and to show how with change of cutoffs parity metrics change for single model/explainer.
#' It is highly suggested to pick small number of metrics. With value of cutoff all subgroup cutoffs change.
#'
#' @param x fairness_object
#' @param grid_points numeric, grid for cutoffs to test. Number of points between 0 and 1 spread evenly.
#' @param fairness_metrics character, name of metric or vector of multiple metrics
#' @param label label of model to be calculated
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
#' ac <- all_cutoffs(fobject, fairness_metrics = c("TPR_parity_loss", "F1_parity_loss"), label = "ranger")
#' plot(ac)
#'

all_cutoffs <- function(x,
                        label,
                        grid_points = 101,
                        fairness_metrics = unique_metrics()){

  stopifnot(class(x) == "fairness_object")

  # error if not in metrics
  lapply(fairness_metrics, assert_parity_metrics)

  if (! is.numeric(grid_points) | length(grid_points) > 1) stop("grid points must be single numeric value")
  if (! is.character(label) | length(label) > 1)  stop("label must be character")

  if (! label %in% x$label ) stop ("label not in provided labels")


  explainers <- x$explainers
  cutoffs    <- seq(0,1, length.out =  grid_points)
  protected  <- x$protected
  privileged <- x$privileged

  n_subgroups <- length(levels(protected))
  cutoff_data <- data.frame()

  # custom cutoffs will give messages (0 in matrices, NA in metrics)  numerous times,
  # so for code below they will be suppressed

  suppressMessages(
  for (custom_cutoff in cutoffs){

      custom_cutoff_vec      <- rep(custom_cutoff, n_subgroups)
      i                      <- match(label, x$label)
      explainer              <- explainers[[i]]


      group_matrices <- group_matrices(protected = protected,
                                       probs = explainer$y_hat,
                                       preds = explainer$y,
                                       cutoff = custom_cutoff_vec)

      # like in create fobject
      gmm             <- calculate_group_fairness_metrics(group_matrices)
      gmm_scaled      <- abs(apply(gmm, 2 , function(x) x  - gmm[,privileged]))
      gmm_loss        <- rowSums(gmm_scaled)
      names(gmm_loss) <- paste0(names(gmm_loss),"_parity_loss")

      gmm_loss_unique <- gmm_loss[names(gmm_loss) %in% fairness_metrics]

      to_add <- data.frame(parity_loss = as.numeric(gmm_loss_unique),
                           metric      = names(gmm_loss_unique),
                           cutoff      = rep(custom_cutoff, length(gmm_loss_unique)),
                           label       = label)

      cutoff_data <- rbind(cutoff_data , to_add)

    })

  all_cutoffs <- list(data = cutoff_data, label = label)
  class(all_cutoffs) <- "all_cutoffs"

  return(all_cutoffs)
}















