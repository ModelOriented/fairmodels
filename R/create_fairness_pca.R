#' Create Fairness PCA
#'
#' @param x fairness object
#' @param data   numerical metric data from code{fairness_object}
#' @param labels labels from metric data from code{fairnss_object}
#'
#' @return fairness pca object
#'
#' @examples
#'
#' library(DALEX)
#' library(ranger)
#'
#' data("compas")
#' y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1
#' # models
#'
#' rf_compas_1 <- ranger(Two_yr_Recidivism ~Number_of_Priors+Age_Below_TwentyFive, data = compas, probability = TRUE) # Wszystko
#' rf_compas_3 <- ranger(Two_yr_Recidivism ~ Sex+Age_Above_FourtyFive, data = compas, probability = TRUE) # dziwny model
#' model_compas_lr <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
#' rf_compas_5 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE) # Wszystko
#' rf_compas_6 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = compas, probability = TRUE) # dziwny model
#' rf_compas_7 <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE) # Wszystko
#' rf_compas_8 <- ranger(Two_yr_Recidivism ~ Sex+Age_Above_FourtyFive+Misdemeanor+Ethnicity, data = compas, probability = TRUE) # dziwny model
#'
#' # explainers
#' explainer_1 <- explain(rf_compas_1, data = compas, y = y_numeric)
#' explainer_3 <- explain(rf_compas_3, data = compas, y = y_numeric)
#' explainer_4 <- explain(model_compas_lr, data = compas, y = y_numeric)
#' explainer_5 <- explain(rf_compas_5, data = compas, y = y_numeric)
#' explainer_6 <- explain(rf_compas_6, data = compas, y = y_numeric)
#' explainer_7 <- explain(rf_compas_7, data = compas, y = y_numeric)
#'
#' # diffrent labels
#' explainer_3$label <- "rf3"
#' explainer_4$label <- "glm"
#' explainer_5$label <- "rf5"
#' explainer_6$label <- "rf6"
#' explainer_7$label <- "rf7"
#' explainers <- list(explainer_1, explainer_3 , explainer_4, explainer_5,explainer_6,explainer_7)
#'
#' fobject <- create_fairness_object(explainer_1,explainer_3,explainer_4,explainer_5,explainer_6,explainer_7,
#'                                   outcome = "Two_yr_Recidivism",
#'                                   group  = "Ethnicity",
#'                                   base   = "Caucasian")
#'
#' create_fairness_pca(fobject)
#'
#' @export
#' @rdname create_fairness_pca


create_fairness_pca <- function(x)  UseMethod("create_fairness_pca")

#' @export
#' @rdname create_fairness_pca
create_fairness_pca.fairness_object <- function(x) {
  stopifnot(class(x) == "fairness_object")

  # extracting metric data from object
  metric_data <- x$metric_data
  n <- ncol(metric_data)

  # normalising
  metric_num_data <- as.data.frame(scale(metric_data[,1:(n-1)]))

  labs <- metric_data[,n]

  create_fairness_object.deafult(data = metric_num_data, labels = labs)
}


#' @export
#' @rdname create_fairness_pca
create_fairness_object.deafult <- function(data, labels){

  pca_fair <- stats::prcomp(data)

  # variances
  pca_summary <- summary(pca_fair)
  pc_1_2 <- round(pca_summary$importance[2,][1:2],2)

  fairness_pca <- list(pc_1_2 = pc_1_2, rotation = pca_fair$rotation, x = pca_fair$x, sdev = pca_fair$sdev , labels = labels)

  class(fairness_pca) <- "fairness_pca"
  return (fairness_pca)

}



