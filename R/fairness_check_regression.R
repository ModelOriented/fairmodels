#' Fairness check regression
#'
#' This is an experimental approach. Please have it in mind when using it.
#' Fairness_check_regression enables to check fairness in regression models. It uses so-called probabilistic classification to approximate fairness measures.
#' The metrics are independence, separation, and sufficiency. The intuition behind this method is that the closer to 1 the metrics are the better.
#' When all metrics are close to 1 then it means that from the perspective of a predictive model there are no meaningful differences between subgroups.
#'
#' @param x object created with \code{\link[DALEX]{explain}} or of class \code{fairness_object_regression}
#' @param ... possibly more objects created with \code{\link[DALEX]{explain}} and/or objects of class \code{fairness_object_regression}
#' @param protected factor, protected variable (also called sensitive attribute), containing privileged and unprivileged groups
#' @param privileged factor/character, one value of \code{protected}, in regard to what subgroup parity loss is calculated
#' @param label character, vector of labels to be assigned for explainers, default is explainer label.
#' @param epsilon numeric, boundary for fairness checking, lowest/maximal acceptable metric values for unprivileged. Default value is 0.8. More on the idea behind epsilon in details section.
#' @param verbose logical, whether to print information about creation of fairness object
#' @param colorize logical, whether to print information in color
#'
#'
#' @rdname fairness_object_regression
#'
#' @return
#'
#' @examples
#'
#'

fairness_check_regression <- function(x,
                                      ...,
                                      protected = NULL,
                                      privileged = NULL,
                                      label = NULL,
                                      epsilon = NULL,
                                      verbose = TRUE,
                                      colorize = TRUE) {



  if (! colorize) {
    color_codes <- list(yellow_start = "", yellow_end = "",
                        red_start = "", red_end = "",
                        green_start = "", green_end = "")
  }


  verbose_cat("Creating fairness regression object\n", verbose = verbose)

  ################  data extraction  ###############

  list_of_objects   <- list(x, ...)
  explainers        <- get_objects(list_of_objects, "explainer")
  fobjects          <- get_objects(list_of_objects, "fairness_object_regression")

  explainers_from_fobjects <- sapply(fobjects, function(x) x$explainers)
  all_explainers           <- append(explainers, explainers_from_fobjects)

  fobjects_fcheck_data <- extract_data(fobjects, "fairness_check_data")

  fobjects_label       <- sapply(fobjects, function(x) x$label)
  n_exp                <- length(explainers)



  ###############  error handling  ###############

  ### protected & privileged

  verbose_cat("-> Privileged subgroup\t\t: ", verbose = verbose)
  privileged <- check_privileged(privileged, fobjects, verbose = verbose)

  verbose_cat("-> Protected variable\t\t:", "factor", "(", verbose = verbose)
  protected <- check_protected(protected, fobjects, verbose = verbose)

  protected_levels <- levels(protected)
  n_lvl            <- length(protected_levels)

  if (! privileged %in% protected_levels) stop("privileged subgroup is not in protected variable vector")

  ############### epsilon ###############
  if (is.null(epsilon)) epsilon <- 0.8
  if (! check_if_numeric_and_single(epsilon)) stop("Epsilon must be single, numeric value")
  if (! check_values(epsilon, 0, 1) )       stop ("epsilon must be within 0 and 1")

  ############### labels ###############

  label <- check_labels(label, explainers, fobjects_label)

  ############## metric calculation ###############

  fairness_check_data <- data.frame()
  for (i in seq_along(explainers)) {
    regression_metrics_data <- regresion_metrics(explainers[[i]], protected, privileged)
    regression_metrics_data['model'] <- label[i]
    fairness_check_data <- rbind(fairness_check_data, regression_metrics_data)

  }

  fairness_check_data <- rbind(fairness_check_data, fobjects_fcheck_data)
  label               <- unlist(c(label, fobjects_label))



  # S3 object
  fairness_object <- list(explainers  = all_explainers,
                          privileged  = privileged,
                          protected   = protected,
                          label       = label,
                          epsilon     = epsilon,
                          fairness_check_data = fairness_check_data)

  verbose_cat(color_codes$green_start, "Fairness object created succesfully", color_codes$green_end, "\n", verbose = verbose)

  class(fairness_object) <- "fairness_object_regression"
  return(fairness_object)

}
