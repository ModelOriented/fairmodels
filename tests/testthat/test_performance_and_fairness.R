test_that("performance_and_fairness with plot",{

  # not many tests because does not work well
  paf <- performance_and_fairness(fobject)

  expect_class(paf, "performance_and_fairness")

  suppressWarnings(  expect_error(performance_and_fairness(fobject, fairness_metric = "non_existing")))
suppressWarnings(  expect_error(performance_and_fairness(fobject, performance_metric = "non_existing")))
suppressWarnings(  expect_error(performance_and_fairness(fairness_metric = c("d","f"))))
suppressWarnings(  expect_error(performance_and_fairness(performance_metric = c("d","f"))))
suppressWarnings(  expect_error(performance_and_fairness(fairness_metric = 17)))
suppressWarnings(  expect_error(performance_and_fairness(performance_metric = 17)))

  plt <- plot(paf)

  expect_class(plt, "ggplot")
  paf <- suppressWarnings(performance_and_fairness(fobject, performance_metric = "auc"))
  paf <- performance_and_fairness(fobject, performance_metric = "accuracy")
  paf <- performance_and_fairness(fobject, performance_metric = "precision")
  paf <- performance_and_fairness(fobject, performance_metric = "recall")
})




