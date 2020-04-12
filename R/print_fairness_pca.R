#' Print fairness PCA
#'
#' @param x fairness object
#' @param ... other params
#'
#' @return
#' @export
#'
#' @examples

print.fairness_pca <- function(x, ...){

  cat("Fairness PCA : \n")
  print(x$x)

  cat("\nCreated with: \n")
  print(as.character(x$labels))

  cat("\nFirst two components explained", sum(x$pc_1_2)*100, "% of variance.")
}
