#' Fairness check regression
#'
#' This is an experimental approach. Please have it in mind when using it.
#' Fairness_check_regression enables to check fairness in regression models. It uses so-called probabilistic classification to approximate fairness measures.
#' The metrics in use are independence, separation, and sufficiency. The intuition behind this method is that the closer to 1 the metrics are the better.
#' When all metrics are close to 1 then it means that from the perspective of a predictive model there are no meaningful differences between subgroups.
#'
#' @param x object created with \code{\link[DALEX]{explain}} or of class \code{fairness_regression_object}.
#' It can be multiple fairness_objects, multiple explainers, or combination on both, as long as
#' they predict the same data. If at least one fairness_object is provided there is no need to
#' pass protected and privileged parameters. Explainers must be of type regression
#' @param ... possibly more objects created with \code{\link[DALEX]{explain}} and/or objects of class \code{fairness_regression_object}
#' @param protected factor, protected variable (also called sensitive attribute), containing privileged and unprivileged groups
#' @param privileged factor/character, one value of \code{protected}, denoting subgroup suspected of the most privilege
#' @param label character, vector of labels to be assigned for explainers, default is explainer label.
#' @param epsilon numeric, boundary for fairness checking, lowest/maximal acceptable metric values for unprivileged. Default value is 0.8.
#' @param verbose logical, whether to print information about creation of fairness object
#' @param colorize logical, whether to print information in color
#'
#' @details
#' Sometimes during metric calculation faze approximation algorithms (logistic regression models) might not coverage properly. This might
#' indicate that the membership to subgroups has strong predictive power.
#'
#' @export
#' @rdname fairness_check_regression
#'
#'
#' @references
#'
#' Steinberg, Daniel & Reid, Alistair & O'Callaghan, Simon. (2020). Fairness Measures for Regression via Probabilistic Classification. - \url{https://arxiv.org/pdf/2001.06089.pdf}
#'
#' @examples
#'
#' set.seed(123)
#' data <- data.frame(
#'   x = c(rnorm(500, 500, 100), rnorm(500, 400, 200)),
#'   pop = c(rep("A", 500), rep("B", 500))
#' )
#'
#' data$y <- rnorm(length(data$x), 1.5 * data$x, 100)
#'
#' # create model
#' model <- lm(y ~ ., data = data)
#'
#' # create explainer
#' exp <- DALEX::explain(model, data = data, y = data$y)
#'
#' # create fobject
#' fobject <- fairness_check_regression(exp, protected = data$pop, privileged = "A")
#'
#' # results
#'
#' fobject
#' plot(fobject)
#' \donttest{
#'
#' model_ranger <- ranger::ranger(y ~ ., data = data, seed = 123)
#' exp2 <- DALEX::explain(model_ranger, data = data, y = data$y)
#'
#' fobject <- fairness_check_regression(exp2, fobject)
#'
#' # results
#' fobject
#'
#' plot(fobject)
#' }
#'
fairness_check_regression <- function(x,
                                      ...,
                                      protected = NULL,
                                      privileged = NULL,
                                      label = NULL,
                                      epsilon = NULL,
                                      verbose = TRUE,
                                      colorize = TRUE) {
  if (!colorize) {
    color_codes <- list(
      yellow_start = "", yellow_end = "",
      red_start = "", red_end = "",
      green_start = "", green_end = ""
    )
  }


  verbose_cat("Creating fairness regression object\n", verbose = verbose)

  ################  data extraction  ###############

  list_of_objects <- list(x, ...)
  explainers <- get_objects(list_of_objects, "explainer")
  fobjects <- get_objects(list_of_objects, "fairness_regression_object")

  explainers_from_fobjects <- sapply(fobjects, function(x) x$explainers)
  all_explainers <- append(explainers, explainers_from_fobjects)

  fobjects_fcheck_data <- extract_data(fobjects, "fairness_check_data")

  fobjects_label <- sapply(fobjects, function(x) x$label)
  n_exp <- length(explainers)


  ############### protected & privileged ###############

  verbose_cat("-> Privileged subgroup\t\t: ", verbose = verbose)
  privileged <- check_privileged(privileged, fobjects, verbose = verbose)

  verbose_cat("-> Protected variable\t\t:", "factor", "(", verbose = verbose)
  protected <- check_protected(protected, fobjects, verbose = verbose)

  protected_levels <- levels(protected)
  n_lvl <- length(protected_levels)

  if (!privileged %in% protected_levels) stop("privileged subgroup is not in protected variable vector")

  ############### epsilon ###############
  if (is.null(epsilon)) epsilon <- 0.8
  if (!check_if_numeric_and_single(epsilon)) stop("Epsilon must be single, numeric value")
  if (!check_values(epsilon, 0, 1)) stop("epsilon must be within 0 and 1")

  ############### explainers & fairness objects ###############

  verbose_cat("-> Fairness objects\t\t:", length(fobjects), verbose = verbose)
  fobjects <- check_fobjects(fobjects, protected, privileged, verbose = verbose)

  verbose_cat("-> Checking explainers\t\t:", length(all_explainers), "in total ", verbose = verbose)
  all_explainers <- check_explainers_reg(all_explainers, protected, verbose = verbose)

  ############### labels ###############

  label <- check_labels(label, explainers, fobjects_label)

  ############## metric calculation ###############

  verbose_cat("-> Metric calculation\t\t: ", verbose = verbose)

  created_na <- FALSE
  regression_metrics_warnings <- NULL
  fairness_check_data <- data.frame()
  for (i in seq_along(explainers)) {
    regression_metrics_obj <- regression_metrics(explainers[[i]], protected, privileged)
    regression_metrics_data <- regression_metrics_obj[[1]]
    regression_metrics_warnings <- regression_metrics_obj[[2]]
    regression_metrics_data["model"] <- label[i]
    fairness_check_data <- rbind(fairness_check_data, regression_metrics_data)
  }

  number_of_metrics_without_NA <- 3
  if (any(is.na(regression_metrics_data))) {
    tmp_data <- regression_metrics_data[c("metric", "score")]
    unique_metrics_with_na <- unique(tmp_data$metric[is.na(tmp_data$score)])
    number_of_metrics_without_NA <- number_of_metrics_without_NA - length(unique_metrics_with_na)
  }

  if (!is.null(regression_metrics_warnings)) {
    verbose_cat(number_of_metrics_without_NA,
      "/3 metrics calculated for all models ( ",
      color_codes$yellow_start,
      "approximation algotithm did not coverage ",
      color_codes$yellow_end,
      " )\n",
      verbose = verbose,
      sep = ""
    )
  } else {
    verbose_cat("3/3 metrics calculated for all models\n",
      verbose = verbose,
      sep = ""
    )
  }

  fairness_check_data <- rbind(fairness_check_data, fobjects_fcheck_data)
  label <- unlist(c(label, fobjects_label))



  # S3 object
  fairness_object <- list(
    explainers = all_explainers,
    privileged = privileged,
    protected = protected,
    label = label,
    epsilon = epsilon,
    fairness_check_data = fairness_check_data
  )

  verbose_cat(color_codes$green_start, "Fairness regression object created succesfully", color_codes$green_end, "\n\n", verbose = verbose)


  class(fairness_object) <- "fairness_regression_object"
  return(fairness_object)
}
