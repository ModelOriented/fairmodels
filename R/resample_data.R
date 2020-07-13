

# resample_data <- function(protected, y, type = "uniform", ... ,  probs){
#
#   stopifnot(is.factor(protected))
#   stopifnot(is.numeric(y))
#   stopifnot(length(y) == length(protected))
#   if (! (all(unique(y) == c(1,0)) | all(unique(y) == c(0,1)) )) stop("y must be numeric vector with values 0 and 1")
#
#   protected_levels <- levels(protected)
#   n                <- length(y)
#   weight_vector    <- rep(NA, n)
#
#   if (type == "preferential"){
#   if (probs < 0 | probs > 1) stop("probs have values outside [0,1]")
#   }
#
#   protected_levels <- levels(protected)
#   n                <- length(y)
#   weight_list      <- list()
#   # subgroup - class observations
#   num_sc <- list()
#
#   for (subgroup in protected_levels){
#     num_c <- rep(NA, 2)
#     w_vec <- rep(NA, 2)
#
#     for (c in unique(y)){
#        # defining number of observations in groups
#        num_c[c + 1] <-  sum(protected == subgroup & y == c)
#
#        # number of observations in subgroup
#        Xs <- sum(protected == subgroup)
#        # number of observations in class
#        Xc <- sum(y == c)
#        # number of observations in class in subgroup
#        Xsc <- sum(protected == subgroup & y == c)
#        # formula for constructing the weights
#        w_vec[c+1] <- (Xs * Xc)/(n*Xsc)
#
#     }
#     num_sc <- append(num_sc , list(num_c))
#     weight_list <- append(weight_list, list(w_vec))
#   }
#   names(num_sc) <- protected_levels
#   names(weight_list) <- protected_levels
#
#
#
# }































