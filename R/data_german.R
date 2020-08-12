#' Modified German Credit data dataset
#'
#' \code{german} dataset. Data contains information about people and their credit risks.
#'
#' @format A data frame with 1000 rows and 10 variables:
#' \describe{
#'   \item{Risk}{factor, good/bad risk connected with giving the credit. Models should predict this values}
#'   \item{Sex}{factor, male/female , considered to be protected group}
#'   \item{Job}{numeric, job titles converted to integers where 0- unemployed/unskilled, 3- management/ self-employed/highly qualified employee/ officer}
#'   \item{Housing}{factor, rent/own/free where this person lives}
#'   \item{Saving.accounts}{factor, little/moderate/quite rich/rich/not_known, where not_known indicates NA}
#'   \item{Checking.account}{factor, little/moderate/rich/not_known, where not_known indicates NA}
#'   \item{Credit.amount}{numeric, amount of money in credit}
#'   \item{Duration}{numeric, duration of credit}
#'   \item{Purpose}{factor, purpose of credit}
#'   \item{Age}{numeric, age of person that applied for credit}
#' }
#' @name german
#' @docType data
#' @usage data(german)
#'
#' @source Data from kaggle \url{https://www.kaggle.com/kabure/german-credit-data-with-risk/}. The original source is UCL \url{https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)}.
NULL
