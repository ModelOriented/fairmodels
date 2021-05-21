
#' Regression metrics
#'
#' @param explainer
#' @param protected
#' @param privileged
#'
#' @return
#' @export
#'
#' @examples
#'


regresion_metrics <- function(explainer, protected, privileged){

  y <- explainer$y
  y_hat <- explainer$y_hat

  protected_levels <- levels(protected)

  unprivileged_levels <- protected_levels[protected_levels != privileged]

  privileged_indices   <- which(privileged == protected)

  data <- data.frame()
  for (unprivileged in unprivileged_levels){

    unprivileged_indices <- which(unprivileged == protected)
    relevant_indices <- c(privileged_indices, unprivileged_indices)
    new_protected <- protected[relevant_indices]

    a <- rep(0, length(new_protected))
    a[new_protected == privileged] <- 1

    y_u <- scale(y[relevant_indices])
    s_u <- scale(y_hat[relevant_indices])

    p_y_data  <- data.frame(a = a, y = y_u)
    p_s_data  <- data.frame(a = a, s = s_u)
    p_ys_data <- data.frame(a = a, s = s_u, y = y_u)

    p_y  <- glm(a ~., data = p_y_data, family=binomial(link="logit"))
    p_s  <- glm(a ~., data = p_s_data, family=binomial(link="logit"))
    p_ys <- glm(a ~., data = p_ys_data, family=binomial(link="logit"))

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

  return(data)

}
