#' Print Fairness Object
#'
#'
#' @param x fairness object
#' @param ... other parameters
#'
#' @export
#'



print.fairness_object <- function(x, ...){
  cat("Fairness Matrics: \n")
  print(x$metric_data)
  cat("Models explained:\n")

  for (explainer in x$explainers) print(explainer$label)

  cat("\nData: \n")
  print(head(x$data,2))

  cat("\n")
  return(invisible(NULL))
}
