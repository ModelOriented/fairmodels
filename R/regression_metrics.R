
#' Regression metrics
#'
#' @param explainer object created with \code{\link[DALEX]{explain}}
#' @param protected factor, protected variable (also called sensitive attribute), containing privileged and unprivileged groups
#' @param privileged factor/character, one value of \code{protected}, denoting subgroup suspected of the most privilege
#'
#' @importFrom stats binomial
#' @importFrom stats glm
#'
#' @return \code{data.frame}
#' @export
#'
#'


regression_metrics <- function(explainer, protected, privileged){

  stopifnot(explainer$model_info$type == 'regression')

  y <- explainer$y
  y_hat <- explainer$y_hat

  protected_levels <- unique(as.character(protected))

  unprivileged_levels <- protected_levels[protected_levels != privileged]

  privileged_indices   <- which(privileged == protected)

  data <- data.frame()
  for (unprivileged in unprivileged_levels){

    unprivileged_indices <- which(unprivileged == protected)
    relevant_indices <- c(privileged_indices, unprivileged_indices)
    new_protected <- as.character(protected[relevant_indices])

    a <- rep(0, length(new_protected))
    a[new_protected == privileged] <- 1

    y_u <- scale(y[relevant_indices])
    s_u <- scale(y_hat[relevant_indices])

    p_y_data  <- data.frame(a = a, y = y_u)
    p_s_data  <- data.frame(a = a, s = s_u)
    p_ys_data <- data.frame(a = a, s = s_u, y = y_u)


    glm_without_warnings <- function(data) {
      r <-
        tryCatch(
          withCallingHandlers(
            {
              warnings_raised <- NULL
              list(value = glm(a ~.,
                               data = data,
                               family=binomial(link="logit")),
                   warnings_raised = warnings_raised)
            },
            warning = function(e){
              warnings_raised <<- trimws(paste0("WARNING: ", e))
              invokeRestart("muffleWarning")
            }
          ))
      return(r)
    }

    p_y_obj <- glm_without_warnings(p_y_data)
    p_s_obj <- glm_without_warnings(p_s_data)
    p_ys_obj <- glm_without_warnings(p_ys_data)

    p_y <- p_y_obj$value
    p_s <- p_s_obj$value
    p_ys <- p_ys_obj$value

    warnings_p_y <- p_y_obj$warnings_raised
    warnings_p_s <- p_s_obj$warnings_raised
    warnings_p_ys <- p_ys_obj$warnings_raised

    warnings_raised <- c(warnings_p_y, warnings_p_s, warnings_p_ys)

    pred_p_y  <- p_y$fitted.values
    pred_p_s  <- p_s$fitted.values
    pred_p_ys <- p_ys$fitted.values

    n <- length(a)

    r_ind <- (n-sum(a))/sum(a) * mean(pred_p_s/(1-pred_p_s))
    r_sep <- mean((pred_p_ys / (1 - pred_p_ys) * (1 - pred_p_y) / pred_p_y))
    r_suf <- mean((pred_p_ys / (1 - pred_p_ys)) * ((1 - pred_p_s) / pred_p_s))

    data_ind <- data.frame(subgroup = unprivileged,
                           score = r_ind,
                           metric = 'independence')

    data_sep <- data.frame(subgroup = unprivileged,
                           score = r_sep,
                           metric = 'separation')

    data_suf <- data.frame(subgroup = unprivileged,
                           score = r_suf,
                           metric = 'sufficiency')


    data <- rbind(data, data_ind, data_sep, data_suf)

  }

  return(list(data, warnings_raised))

}
