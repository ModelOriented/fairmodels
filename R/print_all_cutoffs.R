#' Print all cutoffs
#'
#' @param x all_cuttofs object
#' @param ... other print parameters
#' @param label character, label of model to plot. Default NULL. If default prints all models.
#'
#' @export
#'
#' @importFrom utils head
#'
#' @rdname print_all_cutoffs
#'
#' @examples
#'
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200)
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm, explainer_rf,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' ac <- all_cutoffs(fobject,
#'                   fairness_metrics = c("TPR",
#'                                        "FPR"))
#' print(ac)
#'

print.all_cutoffs <- function(x, ..., label = NULL){

  if (is.null(label)){
    data <- x$cutoff_data
  } else {
    if (! is.character(label) | length(label) > 1)  stop("label must be character")
    data <- x$cutoff_data[x$cutoff_data$label == label, ]
  }

  label <- unique(data$label)

  cat("\nAll cutofs for models:\n", paste(label, collapse = ", "), "\n")
  cat("\nFirst rows from data: \n")
  print(head(data), ...)

  cat("\n")
  return(invisible(NULL))
}
