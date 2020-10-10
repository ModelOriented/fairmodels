#' Ceteris paribus cutoff
#'
#' Ceteris paribus cutoff is way to check how will parity loss behave if only cutoff for one subgroup was changed.
#' By using parameter \code{new_cutoffs} parity loss for metrics with new cutoffs will be calculated. Note that cutoff for subgroup (passed as parameter) will
#' change no matter \code{new_cutoff}'s value at that position. When parameter \code{cumulated} is set to true, all metrics will be summed and facets will
#' collapse to one plot with different models on it. Sometimes due to the fact that some metric might contain NA for all cutoff values, cumulated plot might be present without
#' this model.
#'
#'
#' @param x object of class \code{fairness_object}
#' @param subgroup character, name of subgroup (level in protected variable)
#' @param new_cutoffs list of cutoffs with names matching those of subgroups. Each value should represent cutoff for particular subgroup.
#' Position corresponding to subgroups in levels will be changed. Default is NULL
#' @param fairness_metrics character, name of parity_loss metric or vector of multiple metrics, for full metric names check \code{fairness_check} documentation.
#' @param grid_points numeric, grid for cutoffs to test. Number of points between 0 and 1 spread evenly.
#' @param cumulated logical, if \code{TRUE}  facets will collapse to one plot and parity loss for each model will be summed. Default \code{FALSE}.
#'
#' @return \code{ceteris_paribus_cutoff} \code{data.frame} containing information about label, metric and parity_loss at particular cutoff
#' @export
#'
#' @examples
#' data("compas")
#'
#' # positive outcome - not being recidivist
#' two_yr_recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
#' y_numeric <- as.numeric(two_yr_recidivism) -1
#' compas$Two_yr_Recidivism <- two_yr_recidivism
#'
#' lm_model <- glm(Two_yr_Recidivism~.,
#'                 data=compas,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Two_yr_Recidivism ~.,
#'                            data = compas,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = compas[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = compas[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = compas$Ethnicity,
#'                           privileged = "Caucasian")
#'
#' cpc <- ceteris_paribus_cutoff(fobject, "African_American")
#'
#' plot(cpc)
#'

ceteris_paribus_cutoff <- function(x,
                                   subgroup,
                                   new_cutoffs = NULL,
                                   fairness_metrics = c('ACC', 'TPR', 'PPV', 'FPR', 'STP'),
                                   grid_points = 101,
                                   cumulated = FALSE){

  stopifnot(class(x) == "fairness_object")
  if (! subgroup %in% x$protected) stop("subgroup is not in protected variable")
  # error if not in metrics
  lapply(fairness_metrics, assert_parity_metrics)

  if (! check_if_numeric_and_single(grid_points)) stop("grid points must be single numeric value")

  explainers <- x$explainers
  n_exp      <- length(explainers)
  cutoffs    <- seq(0,1, length.out =  grid_points)
  protected  <- x$protected
  privileged <- x$privileged

  n_subgroups <- length(levels(protected))
  cutoff_data <- data.frame()
  cumulated_data <- data.frame()
  protected_levels <- levels(protected)

  if (is.list(new_cutoffs)){

    if (length(unique(names(new_cutoffs))) != length(names(new_cutoffs))) stop("Names of new_cutoffs list must be unique")
    if (! check_names_in_names_vector(new_cutoffs, protected_levels))  stop("Names of new_cutoffs list does not match levels in protected")
    if (any(! is.numeric(unlist(new_cutoffs)))) stop("Elements of new_cutoffs list must be numeric")
    if (! check_values(unlist(new_cutoffs), 0, 1)) stop("new_cutoffs value must be between 0 and 1")


    # if only few new_cutoffs were provided, fill rest with default 0.5
    if (! all(protected_levels %in% names(new_cutoffs))){
      rest_of_levels <- protected_levels[! (protected_levels == names(new_cutoffs))]
      for (rl in rest_of_levels){
        new_cutoffs[[rl]] <- 0.5
      }
    }
 }

  # custom cutoffs will give messages (0 in matrices, NA in metrics)  numerous times,
  # so for code below they will be suppressed

  cutoff <- NULL
  parity_loss_metric_data       <- matrix(nrow = n_exp, ncol = 12)
  suppressMessages(
    for (i in seq_along(explainers)){

      for (custom_cutoff in cutoffs){

        explainer <- explainers[[i]]

        if (is.null(new_cutoffs)){
          custom_cutoff_vec             <- x$cutoff[[i]]
          custom_cutoff_vec[[subgroup]] <- custom_cutoff
        } else {
          custom_cutoff_vec <- new_cutoffs
          custom_cutoff_vec[[subgroup]] <- custom_cutoff
        }

        label <- x$label[i]

        group_matrices <- group_matrices(protected = protected,
                                         preds     = explainer$y,
                                         probs     = explainer$y_hat,
                                         cutoff    = custom_cutoff_vec)

        # like in create fairness_check
        gmm             <- calculate_group_fairness_metrics(group_matrices)
        parity_loss     <- calculate_parity_loss(gmm, privileged)

        parity_loss_unique <- parity_loss[names(parity_loss) %in% fairness_metrics]

        cum_data <- data.frame(parity_loss = sum(as.numeric(parity_loss_unique)),
                             cutoff      = rep(custom_cutoff, length(parity_loss_unique)),
                             model       = rep(label, length(parity_loss_unique)))
        cumulated_data <- rbind(cumulated_data, cum_data)

        to_add <- data.frame(parity_loss = as.numeric(parity_loss_unique),
                             metric      = names(parity_loss_unique),
                             cutoff      = rep(custom_cutoff, length(parity_loss_unique)),
                             model       = rep(label, length(parity_loss_unique))   )

        cutoff_data <- rbind(cutoff_data , to_add)

    }
    })

  if (cumulated) cutoff_data <- cumulated_data

  models <- unique(cumulated_data$model)
  minimums_list <- list(rep(NA, length(models)))

  for (i in seq_along(models)){
    min_arg <- which.min(cumulated_data[cumulated_data$model == models[i], "parity_loss"])
    minimums_list[i] <- ifelse(length(min_arg) == 0, NA,  cutoff_data$cutoff[min_arg])
  }

  names(minimums_list) <- models

  min_data <- data.frame(model = names(minimums_list), mins = unlist(minimums_list))

  ceteris_paribus_cutoff <- list(cutoff_data = cutoff_data,
                                 subgroup    = subgroup,
                                 cumulated   = cumulated,
                                 label       = x$label,
                                 min_data    = min_data)
  class(ceteris_paribus_cutoff) <- "ceteris_paribus_cutoff"

  return(ceteris_paribus_cutoff)
}





