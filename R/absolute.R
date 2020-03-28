#' Absolute
#'
#' @param x fainress object
#'
#' @return summed absolute difference betwen ideal measure (1) per group
#'
#' @export
#' @rdname absolute

absolute <- function(x) UseMethod("absolute")

# change later
#' @export
#' @rdname absolute

absolute.list <- function(x){
  absolute.deafult(x)
}

#' @export
#' @rdname absolute

absolute.deafult <- function(x){
  return(sum(abs(1- x$Metric[2,])))
}
