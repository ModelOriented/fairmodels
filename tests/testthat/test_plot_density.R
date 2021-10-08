test_that("Test plot_density", {
  glm_compas <- glm(Two_yr_Recidivism ~ ., data = compas, family = binomial(link = "logit"))
  y_numeric <- as.numeric(compas$Two_yr_Recidivism) - 1
  explainer <- DALEX::explain(glm_compas, data = compas, y = y_numeric)

  fobject <- fairness_check(explainer,
    protected = compas$Ethnicity,
    privileged = "Caucasian"
  )

  plt <- plot_density(fobject)

  expect_s3_class(plt, "ggplot")
  expect_equal(plt$labels$x, "probability")
  # no bias
  set.seed(123)
  data <- data.frame(
    x = c(rnorm(500, 500, 100), rnorm(500, 500, 200)),
    pop = c(rep("A", 500), rep("B", 500))
  )

  data$y <- rnorm(length(data$x), 1.5 * data$x, 100)


  model <- lm(y ~ ., data = data)
  exp <- explain(model, data = data, y = data$y)

  protected <- data$pop
  privileged <- "A"

  fobject <- fairness_check_regression(exp, protected = data$pop, privileged = "A")

  model <- ranger(y ~ ., data = data)
  exp <- explain(model, data = data, y = data$y)

  protected <- data$pop
  privileged <- "A"

  fobject <- fairness_check_regression(exp, fobject)

  plt <- plot_density(fobject)

  expect_s3_class(plt, "ggplot")
  expect_equal(plt$labels$x, "predicted values")
})
