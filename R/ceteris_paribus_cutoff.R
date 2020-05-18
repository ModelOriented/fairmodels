#' Ceteris paribus cutoff
#'
#' @description Ceteris paribus cutoff is way to check how will parity loss behave if we changed only cutoff in one subgroup.
#' By using parameter new_cutoffs parity loss for metric's with new cutoffs will be calculated. Note that cutoff for subgroup will
#' change no metter if in new_cutoff is some value. When parameter cummulated is set to true, all metrics will be summed and facets will
#' collapse to one plot with different models on it. Sometimes due to NA's present in some metrics it is needed to drop some metrics.
#'
#'
#' @param x fairness_object
#' @param subgroup character, name of subgroup (level in group)
#' @param new_cutoffs numeric, vector of new cutoffs, length should be equal to number of group levels.
#' Position corresponding to subgroups in levels will be changed. Deafult NULL
#' @param fairness_metrics character, name of metric or vector of metrics
#' @param grid_points numeric, grid for cutoffs to test. Number of points between 0 and 1 spread evenly.
#' @param cummulated logical, if true facets will collapse to one plot and parity loss for each model will be summed. Deafult FALSE.
#'
#' @return
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
#' cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American")
#' plot(cpc)
#'
#'
#' cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American",
#'                               cummulated = TRUE,
#'                               fairness_metrics = c("TPR_parity_loss","PPV_parity_loss", "TNR_parity_loss" ))
#' plot(cpc)
#'

ceteris_paribus_cutoff <- function(x,
                                   subgroup,
                                   new_cutoffs = NULL,
                                   fairness_metrics = unique_metrics(),
                                   grid_points = 101,
                                   cummulated = FALSE){

  stopifnot(class(x) == "fairness_object")

  # error if not in metrics
  lapply(fairness_metrics, assert_parity_metrics)

  if (! is.numeric(grid_points) | length(grid_points) > 1) stop("grid points must be single numeric value")

  explainers <- x$explainers
  cutoffs    <- seq(0,1, length.out =  grid_points)
  data       <- x$data
  group      <- x$group
  outcome    <- x$outcome
  base       <- x$base
  cutoff     <- x$cutoff

  n_subgroups <- length(x$cutoff)
  cutoff_data <- data.frame()


  group_levels <- levels(data[,group])

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
          custom_cutoff_vec  <- cutoff
          custom_cutoff_vec[position_of_subgroup] <- custom_cutoff
        } else {
          custom_cutoff_vec <- new_cutoffs
          custom_cutoff_vec[position_of_subgroup] <- custom_cutoff
        }

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

        if (cummulated){
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

  ceteris_paribus_cutoff <- list(data = cutoff_data, subgroup = subgroup, cummulated = cummulated)
  class(ceteris_paribus_cutoff) <- "ceteris_paribus_cutoff"

  return(ceteris_paribus_cutoff)
}





