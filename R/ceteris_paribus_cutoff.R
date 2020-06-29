#' Ceteris paribus cutoff
#'
#' @description Ceteris paribus cutoff is way to check how will parity loss behave if we changed only cutoff in one subgroup.
#' By using parameter new_cutoffs parity loss for metric's with new cutoffs will be calculated. Note that cutoff for subgroup (passed as parameter) will
#' change no matter new_cutoff's value at that position. When parameter cumulated is set to true, all metrics will be summed and facets will
#' collapse to one plot with different models on it. Sometimes due to NA's present in certain metrics it is advised that those should be omitted.
#'
#'
#' @param x fairness_object
#' @param subgroup character, name of subgroup (level in protected variable)
#' @param new_cutoffs numeric, vector of new cutoffs, length should be equal to number of group levels.
#' Position corresponding to subgroups in levels will be changed. Default is NULL
#' @param fairness_metrics character, name of metric or vector of multiple metrics
#' @param grid_points numeric, grid for cutoffs to test. Number of points between 0 and 1 spread evenly.
#' @param cumulated logical, if \code{TRUE}  facets will collapse to one plot and parity loss for each model will be summed. Default \code{FALSE}.
#'
#' @return
#' @export
#'
#' @examples
#' data("compas")
#'
#' # positive outcome - not being recidivist
#' two_yr_recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))
#' y_numeric <- as.numeric(two_yr_recidivism) -1
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
                                   fairness_metrics = unique_metrics(),
                                   grid_points = 101,
                                   cumulated = FALSE){

  stopifnot(class(x) == "fairness_object")

  # error if not in metrics
  lapply(fairness_metrics, assert_parity_metrics)

  if (! is.numeric(grid_points) | length(grid_points) > 1) stop("grid points must be single numeric value")

  explainers <- x$explainers
  cutoffs    <- seq(0,1, length.out =  grid_points)
  protected  <- x$protected
  privileged <- x$privileged

  n_subgroups <- length(levels(protected))
  cutoff_data <- data.frame()

  group_levels <- levels(protected)

  if (! subgroup %in% group_levels) stop("subgroup is not present in group's levels")

  if (! is.null(new_cutoffs)){
    if (! is.numeric(new_cutoffs) |  length(new_cutoffs) != length(cutoff) ){
      stop("new_cutoffs must be numeric vector of lenght standard cutoffs -1")
    }
  }

  # position of subgroup in group (for cutoff position)
  position_of_subgroup <-  which(subgroup == group_levels)

  # custom cutoffs will give messages (0 in matrices, NA in metrics)  numerous times,
  # so for code below they will be suppressed

  suppressMessages(
    for (i in seq_along(explainers)){

      for (custom_cutoff in cutoffs){

        explainer <- explainers[[i]]

        if (is.null(new_cutoffs)){
          custom_cutoff_vec  <- x$cutoff[[i]]
          custom_cutoff_vec[position_of_subgroup] <- custom_cutoff
        } else {
          custom_cutoff_vec <- new_cutoffs
          custom_cutoff_vec[position_of_subgroup] <- custom_cutoff
        }

        label <- x$label[i]

        group_matrices <- group_matrices(protected = protected,
                                         preds     = explainer$y,
                                         probs     = explainer$y_hat,
                                         cutoff = custom_cutoff_vec)

        # like in create fairness_check
        gmm             <- calculate_group_fairness_metrics(group_matrices)
        gmm_scaled      <- abs(apply(gmm, 2 , function(x) x  - gmm[,privileged]))
        gmm_loss        <- rowSums(gmm_scaled)
        names(gmm_loss) <- paste0(names(gmm_loss),"_parity_loss")

        gmm_loss_unique <- gmm_loss[names(gmm_loss) %in% fairness_metrics]

        if (cumulated){
          to_add <- data.frame(parity_loss = sum(as.numeric(gmm_loss_unique)),
                               cutoff      = rep(custom_cutoff, length(gmm_loss_unique)),
                               model       = rep(label, length(gmm_loss_unique)))

          cutoff_data <- rbind(cutoff_data , to_add)

        } else {
        to_add <- data.frame(parity_loss = as.numeric(gmm_loss_unique),
                             metric      = names(gmm_loss_unique),
                             cutoff      = rep(custom_cutoff, length(gmm_loss_unique)),
                             model       = rep(label, length(gmm_loss_unique))   )

        cutoff_data <- rbind(cutoff_data , to_add)
        }
    }
    })

  ceteris_paribus_cutoff <- list(cutoff_data = cutoff_data,
                                 subgroup    = subgroup,
                                 cumulated   = cumulated,
                                 label       = x$label)
  class(ceteris_paribus_cutoff) <- "ceteris_paribus_cutoff"

  return(ceteris_paribus_cutoff)
}





