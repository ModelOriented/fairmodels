#' Plot fairness radar
#'
#' @description Makes radar plot showing different fairness metrics that allow to compare models.
#'
#' @param x \code{fairness_radar} object
#' @param ... other plot parameters
#'
#' @import ggplot2
#'
#' @return \code{ggplot2} object
#' @export
#' @rdname plot_fairness_radar
#'
#' @references code based on ModelOriented auditor package, thanks agosiewska!  \url{https://modeloriented.github.io/auditor/}
#'
#' @examples
#' data("german")
#'
#' y_numeric <- as.numeric(german$Risk) -1
#'
#' lm_model <- glm(Risk~.,
#'                 data = german,
#'                 family=binomial(link="logit"))
#'
#' explainer_lm <- DALEX::explain(lm_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_lm,
#'                           protected = german$Sex,
#'                           privileged = "male")
#'
#' fradar <- fairness_radar(fobject, fairness_metrics = c("ACC", "STP", "TNR",
#'                                                        "TPR", "PPV"))
#'
#' plot(fradar)
#'
#' \donttest{
#'
#' rf_model <- ranger::ranger(Risk ~.,
#'                            data = german,
#'                            probability = TRUE,
#'                            num.trees = 200,
#'                            num.threads = 1)
#'
#'
#' explainer_rf <- DALEX::explain(rf_model, data = german[,-1], y = y_numeric)
#'
#' fobject <- fairness_check(explainer_rf, fobject)
#'
#'
#' fradar <- fairness_radar(fobject, fairness_metrics = c("ACC", "STP", "TNR",
#'                                                        "TPR", "PPV"))
#'
#' plot(fradar)
#' }
#'

plot.fairness_radar <- function(x, ...) {

  data      <- x$radar_data
  n         <- length(unique(data$model))
  max_score <- max(data$score)
  data$score<- data$score/max_score
  labels    <- round(seq(0, 1, 0.25)*max_score, 2)

  df_text <- data.frame(x = rep(data$parity_loss_metric[1],5), y = c(0.01, 0.25, 0.50, 0.75, 1), label = labels)

  # global variables
  parity_loss_metric <- score <- model <- y <- label <-  NULL
  # plot
  p <- ggplot(data = data, aes(x = parity_loss_metric, y = score)) +
    coord_radar(names_n = length(unique(data$parity_loss_metric))) +
    geom_polygon(aes(group = model, color = model), fill = NA, show.legend = FALSE) +
    geom_point(aes(group = model, color = model)) +
    geom_text(data = df_text, aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(data$score))) +
    scale_color_manual(values = DALEX::colors_discrete_drwhy(n = n)) +
    xlab("") +
    ylab("") +
    ggtitle("Parity loss metric radar plot") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 8),
          plot.title = element_text(color = "#371ea3", face = "bold", hjust = 0.5))

  p
}


# Code from Model's oriented auditor package , thanks agosiewska!
coord_radar <- function(names_n = 2) {

  ggproto("CordRadar", CoordPolar, theta = "x", r = "y", start = - pi / names_n,
          direction = 1, is_linear = function() TRUE, render_bg = render_bg_function)
}

rename_data <- function(coord, data) {
  names(data)[which(colnames(data) == "y")] <- "r"
  names(data)[which(colnames(data) == "x")] <- "theta"
  data
}



theta_rescale <- function(coord, x, scale_details) {
  rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
  rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
}


r_rescale <- function(coord, x, scale_details) {
  scales::rescale(x, c(0, 0.4), scale_details$r.range)
}

render_bg_function <- function(self, scale_details, theme) {
  scale_details <- rename_data(self, scale_details)

  theta <- if (length(scale_details$theta.major) > 0)
    theta_rescale(self, scale_details$theta.major, scale_details)
  thetamin <- if (length(scale_details$theta.minor) > 0)
    theta_rescale(self, scale_details$theta.minor, scale_details)
  thetafine <- seq(0, 2 * pi, length.out = 100)

  rfine <- c(r_rescale(self, scale_details$r.major, scale_details))

  majortheta <- paste("panel.grid.major.", self$theta, sep = "")
  minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
  majorr     <- paste("panel.grid.major.", self$r,     sep = "")

  ggname <- get("ggname", envir = asNamespace("ggplot2"), inherits = FALSE)
  element_render <- get("element_render", envir = asNamespace("ggplot2"), inherits = FALSE)

  ggname("grill", grid::grobTree(
    element_render(theme, "panel.background"),
    if (length(theta) > 0) element_render(
      theme, majortheta, name = "angle",
      x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
      y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
      id.lengths = rep(2, length(theta)),
      default.units = "native"
    ),
    if (length(thetamin) > 0) element_render(
      theme, minortheta, name = "angle",
      x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
      y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
      id.lengths = rep(2, length(thetamin)),
      default.units = "native"
    ),

    element_render(
      theme, majorr, name = "radius",
      x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
      y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
      id.lengths = rep(length(thetafine), length(rfine)),
      default.units = "native"
    )
  ))
}

