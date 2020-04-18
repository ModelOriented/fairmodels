test_that("Calculating cummulated metrics", {
  fake_explainer <- list(nothing = "nothing", y_hat = c(0.32, 0.53,0.94,0.01, 0.32,0.53,0.67,0.01), label = "tester")
  class(fake_explainer) <- "explainer"

  # all metrics should be equal
  test_data <- data.frame(target = factor(c(0,1,0,1,0,1,0,1), levels = c(1,0)),
                          sex = factor(c(rep("male",4), rep("female",4))))

  test_fobject <- create_fairness_object(fake_explainer,
                                         data = test_data,
                                         outcome = "target",
                                         group = "sex",
                                         base = "female")
  m <- ncol(test_fobject$metric_data)

  expect_true(is.na(test_fobject$metric_data$mcc_parity))
  # without NA
  expect_equal(sum(test_fobject$metric_data[,1:7]), 0)

  expect_equal(as.character(test_fobject$metric_data[,m]), "tester")


})
