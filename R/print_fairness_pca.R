#' Print fairness PCA
#'
#' @description Print principal components after using pca on fairness object
#'
#' @param x \code{fairness_pca} object
#' @param ... other \code{fairness_pca} objects
#'
#' @import utils
#'
#' @export
#' @rdname print_fairness_pca
#'
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
#'  # same explainers with different cutoffs for female
#' fobject <- fairness_check(explainer_lm, explainer_rf, fobject,
#'                           protected = german$Sex,
#'                           privileged = "male",
#'                           cutoff = c(0.4,0.5),
#'                           label = c("lm_2", "rf_2"))
#'
#' fpca <- fairness_pca(fobject)
#'
#' print(fpca)
#'


print.fairness_pca <- function(x, ...){

  cat("Fairness PCA : \n")
  print(x$x)

  cat("\nCreated with: \n")
  print(as.character(x$label))

  cat("\nFirst two components explained", sum(x$pc_1_2)*100, "% of variance.\n")

  return(invisible(NULL))
}

