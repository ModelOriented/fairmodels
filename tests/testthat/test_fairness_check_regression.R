test_that("Test fairness_check_regression", {


  # no bias
  data <- data.frame(x   = c(rnorm(500, 500, 100), rnorm(500, 500, 200)),
                     pop = c(rep('A', 500 ), rep('B', 500 )))

  data$y <- rnorm(length(data$x), 1.5 * data$x, 100)


  model <- lm(y~., data = data)
  exp <- explain(model, data = data, y = data$y)

  protected <- data$pop
  privileged <- 'A'

  fobject <- fairness_check_regression(exp, protected = data$pop, privileged = 'A')

  expect_equal(fobject$privileged, 'A')
  expect_equal(fobject$fairness_check_data,
               fairness_check_regression(exp, protected = data$pop,
                                         privileged = 'A', colorize = FALSE)$fairness_check_data)

  expect_equal(fobject$protected, as.factor(data$pop))
  expect_equal(fobject$label, 'lm')
  expect_equal(sort(as.character(fobject$fairness_check_data$metric)), c("independence", "separation", "sufficiency" ))
  expect_equal(fobject$epsilon, 0.8)


  fobject <- fairness_check_regression(exp,
                                       protected = data$pop,
                                       privileged = 'A',
                                       label = 'test',
                                       epsilon = 0.45)

  expect_equal(fobject$epsilon, 0.45)
  expect_equal(fobject$label, 'test')

  # label error
  expect_error(fairness_check_regression(exp, fobject,
                                         protected = data$pop,
                                         privileged = 'A',
                                         label = 'test'))
  # protected error
  expect_error(fairness_check_regression(exp, fobject,
                                         protected = data$x,
                                         privileged = 'A'))

  expect_error(fairness_check_regression(exp, fobject,
                                         protected = data$pop,
                                         privileged = 'not existing'))

  expect_error(fairness_check_regression(exp, fobject, epsilon = 10))

  expect_error(fairness_check_regression(exp, fobject, epsilon = c(0.1, 0.2)))

  # wrong model type
  exp2 <- exp
  exp2$model_info$type <- 'classification'
  expect_error(fairness_check_regression(exp2, fobject,
                                         protected = data$pop))


  # did not coverage
  exp$y <- seq(-15,15, length.out = 1000)
  exp$y_hat <- c(rep(-4000, 500), rep(5000, 500))

  expect_equal(capture.output(fairness_check_regression(exp, protected = data$pop, privileged = 'A'))[6],
               '-> Metric calculation\t\t: 3/3 metrics calculated for all models ( \033[33mapproximation algotithm did not coverage \033[39m )')

  exp <- explain(model, data = data, y = data$y)
  fobject <- fairness_check_regression(exp, protected = data$pop, privileged = 'A')
  fobject <- fairness_check_regression(exp, fobject, label = 'lm2')

  plt <- plot(fobject)

  expect_equal(plt$labels$subtitle, "Created with lm2, lm")
  expect_equal(plt$labels$title , "Fairness check regression")
  expect_s3_class(plt, "ggplot")

  })
