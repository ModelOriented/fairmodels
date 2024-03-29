% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fairness_check_regression.R
\name{fairness_check_regression}
\alias{fairness_check_regression}
\title{Fairness check regression}
\usage{
fairness_check_regression(
  x,
  ...,
  protected = NULL,
  privileged = NULL,
  label = NULL,
  epsilon = NULL,
  verbose = TRUE,
  colorize = TRUE
)
}
\arguments{
\item{x}{object created with \code{\link[DALEX]{explain}} or of class \code{fairness_regression_object}.
It can be multiple fairness_objects, multiple explainers, or combination on both, as long as
they predict the same data. If at least one fairness_object is provided there is no need to
pass protected and privileged parameters. Explainers must be of type regression}

\item{...}{possibly more objects created with \code{\link[DALEX]{explain}} and/or objects of class \code{fairness_regression_object}}

\item{protected}{factor, protected variable (also called sensitive attribute), containing privileged and unprivileged groups}

\item{privileged}{factor/character, one value of \code{protected}, denoting subgroup suspected of the most privilege}

\item{label}{character, vector of labels to be assigned for explainers, default is explainer label.}

\item{epsilon}{numeric, boundary for fairness checking, lowest/maximal acceptable metric values for unprivileged. Default value is 0.8.}

\item{verbose}{logical, whether to print information about creation of fairness object}

\item{colorize}{logical, whether to print information in color}
}
\description{
This is an experimental approach. Please have it in mind when using it.
Fairness_check_regression enables to check fairness in regression models. It uses so-called probabilistic classification to approximate fairness measures.
The metrics in use are independence, separation, and sufficiency. The intuition behind this method is that the closer to 1 the metrics are the better.
When all metrics are close to 1 then it means that from the perspective of a predictive model there are no meaningful differences between subgroups.
}
\details{
Sometimes during metric calculation faze approximation algorithms (logistic regression models) might not coverage properly. This might
indicate that the membership to subgroups has strong predictive power.
}
\examples{

set.seed(123)
data <- data.frame(
  x = c(rnorm(500, 500, 100), rnorm(500, 400, 200)),
  pop = c(rep("A", 500), rep("B", 500))
)

data$y <- rnorm(length(data$x), 1.5 * data$x, 100)

# create model
model <- lm(y ~ ., data = data)

# create explainer
exp <- DALEX::explain(model, data = data, y = data$y)

# create fobject
fobject <- fairness_check_regression(exp, protected = data$pop, privileged = "A")

# results

fobject
plot(fobject)
\donttest{

model_ranger <- ranger::ranger(y ~ ., data = data, seed = 123)
exp2 <- DALEX::explain(model_ranger, data = data, y = data$y)

fobject <- fairness_check_regression(exp2, fobject)

# results
fobject

plot(fobject)
}

}
\references{
Steinberg, Daniel & Reid, Alistair & O'Callaghan, Simon. (2020). Fairness Measures for Regression via Probabilistic Classification. - \url{https://arxiv.org/pdf/2001.06089.pdf}
}
