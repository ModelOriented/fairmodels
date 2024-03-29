% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fairness_radar.R
\name{fairness_radar}
\alias{fairness_radar}
\title{Fairness radar}
\usage{
fairness_radar(x, fairness_metrics = c("ACC", "TPR", "PPV", "FPR", "STP"))
}
\arguments{
\item{x}{object of class \code{fairness_object}}

\item{fairness_metrics}{character, vector of metric names, at least 3 metrics without NA needed. Full names of metrics can be found in \code{fairness_check} documentation.}
}
\value{
\code{fairness_radar} object.
It is a list containing:
\itemize{
\item{radar_data}{ - \code{data.frame} containing scores for each model and parity loss metric}
\item{label}{ - model labels}
}
}
\description{
Make \code{fairness_radar} object with chosen \code{fairness_metrics}. Note that there must be at least three metrics that does not contain NA.
}
\examples{
data("german")

y_numeric <- as.numeric(german$Risk) - 1

lm_model <- glm(Risk ~ .,
  data = german,
  family = binomial(link = "logit")
)

explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)

fobject <- fairness_check(explainer_lm,
  protected = german$Sex,
  privileged = "male"
)

fradar <- fairness_radar(fobject, fairness_metrics = c(
  "ACC", "STP", "TNR",
  "TPR", "PPV"
))

plot(fradar)
\donttest{

rf_model <- ranger::ranger(Risk ~ .,
  data = german,
  probability = TRUE,
  num.trees = 200,
  num.threads = 1
)


explainer_rf <- DALEX::explain(rf_model, data = german[, -1], y = y_numeric)

fobject <- fairness_check(explainer_rf, fobject)


fradar <- fairness_radar(fobject, fairness_metrics = c(
  "ACC",
  "STP",
  "TNR",
  "TPR",
  "PPV"
))

plot(fradar)
}

}
