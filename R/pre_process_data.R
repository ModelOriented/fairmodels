#' Pre-process data
#'
#' Function aggregates all pre-processing algorithms for bias mitigation. User passes unified arguments and specifies type to receive transformed \code{data.frame}
#'
#' @param data \code{data.frame}
#' @param protected factor, protected attribute (sensitive variable) containing information about gender, race etc...
#' @param y numeric, numeric values of predicted variable. 1 should denote favorable outcome.
#' @param type character, type of pre-processing algorithm to be used, one of:
#' \itemize{
#' \item{resample_uniform}
#' \item{resample_preferential}
#' \item{reweight}
#' \item{disparate_impact_remover}
#' }
#' @param ... other parameters passed to pre-processing algorithms
#'
#' @return modified data (\code{data.frame}). In case of type = 'reweight' data has feature `_weights_` containing weights that need to be passed to model.
#' In other cases data is ready to be passed as training data to a model.
#' @export
#'
#' @examples
#' data("german")
#'
#' pre_process_data(german,
#'                  german$Sex,
#'                  as.numeric(german$Risk)-1,
#'                  type = "disparate_impact_remover",
#'                  features_to_transform = 'Age')



pre_process_data <- function(data, protected, y,  type = 'resample_uniform', ...){

  switch(type,
         resample_uniform      = { return( data[resample(protected, y, ...), ] )},
         resample_preferential = { return( data[resample(protected, y, type = "preferential", ...), ] )},
         reweight              = {
           data$`_weights_` <- reweight(protected, y)
           return(data)
         },

         disparate_impact_remover = {return( disparate_impact_remover(data, protected, ...))}
  )

  # if not in switch:
  stop("type must be equal to one of supported types, see documentation: ?pre_process_data")
}
