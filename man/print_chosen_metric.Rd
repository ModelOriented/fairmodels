% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.chosen_metric}
\alias{print.chosen_metric}
\title{Print chosen metric}
\usage{
\method{print}{chosen_metric}(x, ...)
}
\arguments{
\item{x}{\code{chosen_metric} object}

\item{...}{other print parameters}
}
\description{
Choose metric from parity loss metrics and plot it for every model.
The one with the least parity loss is more fair in terms of this particular metric.
}
\examples{

data("german")

y_numeric <- as.numeric(german$Risk) - 1

lm_model <- glm(Risk ~ .,
  data = german,
  family = binomial(link = "logit")
)

rf_model <- ranger::ranger(Risk ~ .,
  data = german,
  probability = TRUE,
  num.trees = 200,
  num.threads = 1
)

explainer_lm <- DALEX::explain(lm_model, data = german[, -1], y = y_numeric)
explainer_rf <- DALEX::explain(rf_model, data = german[, -1], y = y_numeric)

fobject <- fairness_check(explainer_lm, explainer_rf,
  protected = german$Sex,
  privileged = "male"
)

cm <- choose_metric(fobject, "TPR")
print(cm)
}
