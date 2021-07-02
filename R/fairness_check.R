#' Fairness check
#'
#' Fairness check creates \code{fairness_object} which measures different fairness metrics and wraps data, explainers and parameters in useful object. This is fundamental object in this package.
#' It enables to visualize fairness metrics and models in many ways and compare models on both fairness and performance level. Fairness check acts as merger and wrapper for explainers and fairness objects.
#' While other fairness objects values are not changed, fairness check assigns cutoffs and labels to provided explainers so same explainers with changed labels/cutoffs might be gradually added to fairness object.
#' Users through print and plot methods may quickly check values of most popular fairness metrics. More on that topic in details.
#'
#' @param x object created with \code{\link[DALEX]{explain}} or of class \code{fairness_object}
#' @param ... possibly more objects created with \code{\link[DALEX]{explain}} and/or objects of class \code{fairness_object}
#' @param protected factor, protected variable (also called sensitive attribute), containing privileged and unprivileged groups
#' @param privileged factor/character, one value of \code{protected}, in regard to what subgroup parity loss is calculated
#' @param cutoff numeric, vector of cutoffs (thresholds) for each value of protected variable, affecting only explainers.
#' @param label character, vector of labels to be assigned for explainers, default is explainer label.
#' @param epsilon numeric, boundary for fairness checking, lowest acceptable ratio of metrics between unprivileged and privileged subgroups. Default value is 0.8. More on the idea behind epsilon in details section.
#' @param verbose logical, whether to print information about creation of fairness object
#' @param colorize logical, whether to print information in color
#'
#' @details
#' Fairness check
#'
#' Metrics used are made for each subgroup, then base metric score is subtracted leaving loss of particular metric.
#' If absolute loss of metrics ratio is not within acceptable boundaries than such metric is marked as "not passed". It means that values of metrics should be within (epsilon, 1/epsilon) boundary.
#' The default ratio is set to 0.8 which adhere to US 80% rule (more on it here: \url{https://en.wikipedia.org/wiki/Disparate_impact#The_80%_rule}). It means that unprivileged subgroups should have at least 80%
#' score achieved in metrics by privileged subgroup. For example if TPR_unprivileged/TPR_privileged is less than 0.8 then such ratio is sign of discrimination. On the other hand if
#' TPR_privileged/TPR_unprivileged is more than 1.25 (1/0.8) than there is discrimination towards privileged group.
#' Epsilon value can be adjusted to user's needs. It should be interpreted as the lowest ratio of metrics allowed.  There are some metrics that might be derived from existing metrics (For example Equalized Odds - equal TPR and FPR for all subgroups).
#' That means passing 5 metrics in fairness check asserts that model is even more fair. In \code{fairness_check} models must always predict positive result. Not adhering to this rule
#' may lead to misinterpretation of the plot. More on metrics and their equivalents:
#' \url{https://fairware.cs.umass.edu/papers/Verma.pdf}
#' \url{https://en.wikipedia.org/wiki/Fairness_(machine_learning)}
#'
#' Parity loss - visualization tool
#'
#' Parity loss is computed as follows:
#' M_parity_loss = sum(abs(log(metric/metric_privileged)))
#'
#' where:
#'
#' M - some metric mentioned above
#'
#' metric - vector of metric scores from each subgroup
#' metric_privileged - value of metric vector for privileged subgroup
#'
#' base_metric - scalar, value of metric for base subgroup
#'
#'
#' @return An object of class \code{fairness_object} which is a list with elements:
#' \itemize{
#' \item parity_loss_metric_data - data.frame containing parity loss for various fairness metrics. Created with following metrics:
#' \itemize{
#'
#' \item{TPR}{ - True Positive Rate (Sensitivity, Recall)}
#' \item{TNR}{ - True Negative Rate (Specificity)}
#' \item{PPV}{ - Positive Predictive Value (Precision)}
#' \item{NPV}{ - Negative Predictive Value}
#' \item{FNR}{ - False Negative Rate}
#' \item{FPR}{ - False Positive Rate}
#' \item{FDR}{ - False Discovery Rate}
#' \item{FOR}{ - False Omission Rate}
#' \item{TS}{ - Threat Score}
#' \item{STP}{ - Statistical Parity}
#' \item{ACC}{ - Accuracy}
#' \item{F1}{ - F1 Score}
#' }
#'
#' \item{groups_data}{ - metrics across levels in protected variable}
#' \item{groups_confusion_matrices}{ - confusion matrices for each subgroup}
#' \item{explainers}{ - list of \code{DALEX} explainers used to create object}
#' \item{cutoffs}{ - list of cutoffs for each explainer and subgroup}
#' \item{fairness_check_data}{ - \code{data.frame} used for for plotting \code{fairness_object}}
#' \item{...}{ - other parameters passed to function}
#' }
#'
#' @references
#' Zafar,Valera, Rodriguez, Gummadi (2017)  \url{https://arxiv.org/pdf/1610.08452.pdf}
#'
#' Hardt, Price, Srebro (2016) \url{https://arxiv.org/pdf/1610.02413.pdf}
#'
#' Verma, Rubin (2018) \url{https://fairware.cs.umass.edu/papers/Verma.pdf}
#'
#' Barocas, Hardt, Narayanan (2019) \url{https://fairmlbook.org/}
#'
#' @export
#' @rdname fairness_check
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
#' plot(fobject)
#'
#' \donttest{
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            max.depth = 3,
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
#' plot(fobject)
#'}


fairness_check <- function(x,
                           ...,
                           protected = NULL,
                           privileged = NULL,
                           cutoff = NULL,
                           label = NULL,
                           epsilon = 0.8,
                           verbose = TRUE,
                           colorize = TRUE) {

  if (! colorize) {
    color_codes <- list(yellow_start = "", yellow_end = "",
                        red_start = "", red_end = "",
                        green_start = "", green_end = "")
  }

  verbose_cat("Creating fairness classification object\n", verbose = verbose)

  ################  data extraction  ###############

  list_of_objects   <- list(x, ...)
  explainers        <- get_objects(list_of_objects, "explainer")
  fobjects          <- get_objects(list_of_objects, "fairness_object")

  explainers_from_fobjects <- sapply(fobjects, function(x) x$explainers)
  all_explainers           <- append(explainers, explainers_from_fobjects)

  fobjects_metric_data <- extract_data(fobjects, "parity_loss_metric_data")
  fobjects_groups_data <- extract_data(fobjects, "groups_data")
  fobjects_fcheck_data <- extract_data(fobjects, "fairness_check_data")
  fobjects_cf          <- extract_data(fobjects, "groups_confusion_matrices")

  fobjects_label       <- unlist(lapply(fobjects, function(x) x$label))
  fobjects_cuttofs     <- extract_data(fobjects, "cutoff")
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

  ############### cutoff handling- if cutoff is null than 0.5 for all subgroups ###############

  verbose_cat("-> Cutoff values for explainers\t: ", verbose = verbose)


  if (is.numeric(cutoff) & length(cutoff) > 1) stop("Please provide cutoff as list with the same names as levels in protected factor")

  if (is.list(cutoff)){

    if (!  check_unique_names(cutoff))                            stop("Names of cutoff list must be unique")
    if (! check_names_in_names_vector(cutoff, protected_levels))  stop("Names of cutoff list does not match levels in protected")
    if (! check_list_elements_numeric(cutoff))                    stop("Elements of cutoff list must be numeric")
    if (! check_values(unlist(cutoff), 0, 1))                     stop("Cutoff value must be between 0 and 1")


    # if only few cutoffs were provided, fill rest with default 0.5
    if (! all(protected_levels %in% names(cutoff))) {
      rest_of_levels <- protected_levels[ ! (protected_levels == names(cutoff))]
      for (rl in rest_of_levels){
        cutoff[[rl]] <- 0.5
      }
    }
   verbose_cat(paste(names(cutoff), ": ", cutoff, collapse = ", ", sep = ""), "\n", verbose = verbose)
  }


  if (check_if_numeric_and_single(cutoff)) {
    if (! check_values(cutoff, 0,1)) stop("Cutoff value must be between 0 and 1")
    cutoff <- as.list(rep(cutoff, n_lvl))
    names(cutoff) <- protected_levels
    verbose_cat(cutoff[[1]], "( for all subgroups )\n", verbose = verbose)
  }

  if (is.null(cutoff)) {
    cutoff <- as.list(rep(0.5, n_lvl))
    names(cutoff) <- protected_levels
    verbose_cat("0.5 ( for all subgroups ) \n", verbose = verbose)
  }


  ############### epsilon ###############
  if (is.null(epsilon)) epsilon <- 0.8
  if (! check_if_numeric_and_single(epsilon)) stop("Epsilon must be single, numeric value")
  if (! check_values(epsilon, 0, 1) )       stop ("epsilon must be within 0 and 1")

  ############### explainers & fairness objects ###############

  verbose_cat("-> Fairness objects\t\t:", length(fobjects), verbose = verbose)
  fobjects <- check_fobjects(fobjects, protected, privileged, verbose = verbose)

  verbose_cat("-> Checking explainers\t\t:", length(all_explainers), "in total ", verbose = verbose)
  all_explainers <- check_explainers_clf(all_explainers, protected, verbose = verbose)


  ############### labels ###############

  label <- check_labels(label, explainers, fobjects_label)

  ###############  fairness metric calculation  ###############

  verbose_cat("-> Metric calculation\t\t: ", verbose = verbose)

  created_na <- FALSE
  # number of metrics must be fixed. If changed add metric to metric labels
  # and change in calculate group fairness metrics
  parity_loss_metric_data       <- matrix(nrow = n_exp, ncol = 12)
  explainers_confusion_matrices <- list(rep(0,n_exp))

  explainers_groups <- list(rep(0,n_exp))
  df                <- data.frame()
  cutoffs           <- as.list(rep(0, n_exp))
  names(cutoffs)    <- label
  parity_loss_names <- NULL

  for (i in seq_along(explainers)) {
    # note that this is along explainers passed to fc, not all_explainers (eg from fairness_objects)
    # those have already calculated metrics and are just glued together
    group_matrices <- group_matrices(protected = protected,
                                     probs = explainers[[i]]$y_hat,
                                     preds = explainers[[i]]$y,
                                     cutoff = cutoff)

    explainers_confusion_matrices[[i]] <- group_matrices

    # storing cutoffs for explainers
    cutoffs[[label[i]]] <- cutoff

    # group metric matrix
    gmm <- calculate_group_fairness_metrics(group_matrices)

    # parity_loss
    parity_loss <- calculate_parity_loss(gmm, privileged)
    parity_loss_metric_data[i, ] <- parity_loss
    parity_loss_names <- names(parity_loss)


    # every group value for every metric for every explainer
    metric_list                 <- lapply(seq_len(nrow(gmm)), function(j) gmm[j,])
    names(metric_list)          <- rownames(gmm)
    explainers_groups[[i]]      <- metric_list
    names(explainers_groups)[i] <- label[i]
    names(explainers_confusion_matrices)[i] <- label[i]

    ###############  fairness check  ###############

    fairness_check_data <- lapply(metric_list, function(y) y / y[privileged])

    # omit base metric because it is always 0
    fairness_check_data <- lapply(fairness_check_data, function(x) x[names(x) != privileged])

    # if metric is 0 change to NA
    fairness_check_data <- lapply(fairness_check_data, function(x) ifelse(x == 0, NA, x))


    statistical_parity_loss   <- fairness_check_data$STP
    equal_oportunity_loss     <- fairness_check_data$TPR
    predictive_parity_loss    <- fairness_check_data$PPV
    predictive_equality_loss  <- fairness_check_data$FPR
    accuracy_equality_loss    <- fairness_check_data$ACC

    n_sub <- n_lvl -1
    n_exp <- length(x$explainers)

    # creating data frames for fairness check

    metric <- c(rep("Accuracy equality ratio    (TP + TN)/(TP + FP + TN + FN)", n_sub),
                rep("Predictive parity ratio     TP/(TP + FP)", n_sub),
                rep("Predictive equality ratio   FP/(FP + TN)", n_sub),
                rep("Equal opportunity ratio     TP/(TP + FN)", n_sub),
                rep("Statistical parity ratio   (TP + FP)/(TP + FP + TN + FN)", n_sub))

    score <- c(unlist(accuracy_equality_loss),
               unlist(predictive_parity_loss),
               unlist(predictive_equality_loss),
               unlist(equal_oportunity_loss),
               unlist(statistical_parity_loss))

    # 5 is number of metrics
    subgroup <- rep(names(accuracy_equality_loss), 5)
    model    <- rep(rep(label[i], n_sub),5)

    df_to_add <- data.frame(score = score,
                            subgroup = subgroup,
                            metric = metric,
                            model = model)

    # add metrics to dataframe
    df <- rbind(df, df_to_add)
  }

  rownames(df) <- NULL
  cols_with_na <- 0
  if (any(is.na(parity_loss_metric_data))){
    created_na <- TRUE
    num_NA <- sum(is.na(parity_loss_metric_data))
    cols_with_na <- sum(apply(parity_loss_metric_data, 2, function(x) any(is.na(x))))
  }

  if (created_na){
    verbose_cat(ncol(parity_loss_metric_data) - cols_with_na,
                "/",
                ncol(parity_loss_metric_data),
                " metrics calculated for all models ( ",
                color_codes$yellow_start, num_NA,
                " NA created",
                color_codes$yellow_end,
                " )\n",
                verbose = verbose,
                sep = "")
  } else {
    verbose_cat(ncol(parity_loss_metric_data) - cols_with_na,
                "/",
                ncol(parity_loss_metric_data),
                " metrics calculated for all models\n",
                verbose = verbose,
                sep = "")

  }

  ###############  Merging with fairness objects  ###############

  # as data frame and making numeric
  parity_loss_metric_data           <- as.data.frame(parity_loss_metric_data)

  if (is.null(parity_loss_names)) parity_loss_names <- names(parity_loss_metric_data)
  colnames(parity_loss_metric_data) <- parity_loss_names



  # merge explainers data with fobjects
  parity_loss_metric_data       <- rbind(parity_loss_metric_data, fobjects_metric_data)
  explainers_groups <- append(explainers_groups, fobjects_groups_data)
  explainers_confusion_matrices <- append(explainers_confusion_matrices, fobjects_cf)
  df                <- rbind(df, fobjects_fcheck_data)
  cutoffs           <- append(cutoffs, fobjects_cuttofs)
  label             <- unlist(c(label, fobjects_label))
  names(cutoffs)           <- label
  names(explainers_groups) <- label
  names(explainers_confusion_matrices) <- label

  # S3 object
  fairness_object <- list(parity_loss_metric_data = parity_loss_metric_data,
                          groups_data = explainers_groups,
                          groups_confusion_matrices = explainers_confusion_matrices,
                          explainers  = all_explainers,
                          privileged  = privileged,
                          protected   = protected,
                          label       = label,
                          cutoff      = cutoffs,
                          epsilon     = epsilon,
                          fairness_check_data = df)

  class(fairness_object) <- "fairness_object"

  verbose_cat(color_codes$green_start, "Fairness object created succesfully", color_codes$green_end, "\n", verbose = verbose)

  return(fairness_object)
}

