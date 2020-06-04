#' Get objects
#'
#' @description Extract certain class objects from a list
#'
#' @param x list of elements
#' @param class character, name of class to be extracted from list
#'
#' @return
#' @export

get_objects <- function(x, class){

  stopifnot(class(x) == "list")

  explainers <- list()
  j <- 1

  for (i in seq_along(x)){
    if (class(x[[i]]) == class) {
      explainers[[j]] <- x[[i]]
      j               <- j + 1
     }
  }

  return(explainers)
}
