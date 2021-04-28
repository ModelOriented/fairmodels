#' All cutoffs
#'
#' Create \code{all_cutoffs} object and see how with the change of cutoffs parity loss of fairness metrics changes. Value of cutoff changes equally for all subgroups.
#' User can pick which fairness metrics to create the object with via fairness_metrics vector.
#'
#' @param x object of class \code{fairness_object}
#' @param grid_points numeric, grid for cutoffs to test. Number of points between 0 and 1 spread evenly
#' @param fairness_metrics character, name of parity_loss metric or vector of multiple metrics names. Full names can be found in \code{fairness_check} documentation.
#'
#' @return \code{all_cutoffs} object, \code{data.frame} containing information about label, metric and parity_loss at particular cutoff
#' @export
#'
#' @examples
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' ac <- all_cutoffs(fobject)
#' plot(ac)
#'
#' \donttest{
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 100,
#'                            seed = 1)
#'
#'
#' explainer_rf <- DALEX::explain(rf_model,
#'                                data = german[,-1],
#'                                y = y_numeric)
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#' ac <- all_cutoffs(fobject)
#'
#' plot(ac)
#' }

all_cutoffs <- function(x,
                        grid_points = 101,
                        fairness_metrics = c('ACC', 'TPR', 'PPV', 'FPR', 'STP')){

  stopifnot(class(x) == "fairness_object")

  # error if not in metrics
  lapply(fairness_metrics, assert_parity_metrics)

  if (! is.numeric(grid_points) | length(grid_points) > 1) stop("grid points must be single numeric value")



  explainers <- x$explainers
  n_exp      <- length(explainers)
  cutoffs    <- seq(0,1, length.out =  grid_points)
  protected  <- x$protected
  privileged <- x$privileged

  n_subgroups <- length(levels(protected))
  cutoff_data <- data.frame()

  # custom cutoffs will give messages (0 in matrices, NA in metrics)  numerous times,
  # so for code below they will be suppressed
  parity_loss_metric_data       <- matrix(nrow = n_exp, ncol = 12)

  suppressMessages(
  for (i in seq_along(explainers)){
  for (custom_cutoff in cutoffs){

      custom_cutoff_vec        <- as.list(rep(custom_cutoff, n_subgroups))
      names(custom_cutoff_vec) <- levels(protected)
      explainer                <- explainers[[i]]


      group_matrices <- group_matrices(protected = protected,
                                       probs = explainer$y_hat,
                                       preds = explainer$y,
                                       cutoff = custom_cutoff_vec)

      # like in create fobject
      gmm            <- calculate_group_fairness_metrics(group_matrices)
      parity_loss    <- calculate_parity_loss(gmm, privileged)
      parity_loss <- parity_loss[names(parity_loss) %in% fairness_metrics]

      to_add <- data.frame(parity_loss = as.numeric(parity_loss),
                           metric      = names(parity_loss),
                           cutoff      = rep(custom_cutoff, length(parity_loss)),
                           label       = x$label[i])

      cutoff_data <- rbind(cutoff_data , to_add)

    }
  })

  all_cutoffs <- list(cutoff_data = cutoff_data)
  class(all_cutoffs) <- "all_cutoffs"

  return(all_cutoffs)
}















