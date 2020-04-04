#' Modified COMPAS dataset (from fairness package)
#'
#' @description
#' \code{compas}
#'
#' \describe{
#'   \item{Two_yr_Recidivism}{factor, 1/0 for future recifivism or no recidivism. Models should predict this values}
#'   \item{Number_of_Priors}{numeric, number of priors, normalized}
#'   \item{Age_Above_FourtyFive}{factor, yes/no for age above 45 years or not}
#'   \item{Age_Below_TwentyFive}{factor, yes/no for age below 25 years or not}
#'   \item{Sex}{factor, female/male for gender}
#'   \item{Misdemeanor}{factor, yes/no for having recorded misdemeanor(s) or not}
#'   \item{ethnicity}{factor, Caucasian, African American, Asian, Hispanic, Native American or Other}
#' }
#' @name compas
#' @docType data
#' @usage data(compas)
#' @format A data frame with 6172 rows and 7 variables:
#'
#' @source Modified data from `fairness` package. Without probabilities and predicted values. The original source of data is Kaggle.
NULL
