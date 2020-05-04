test_that("Test group_metrics ", {


  expect_error(group_metric(fobject, fairness_metric = 1231))
  expect_error(group_metric(fobject, fairness_metric = c("FPR", "TPR")))
  expect_error(group_metric(fobject, fairness_metric = "TPRR"))
  expect_error(group_metric(fobject, performance_metric = 1231))
  expect_error(group_metric(fobject, performance_metric = c("f1", "auc")))
  expect_error(group_metric(fobject, performance_metric = "f11"))


  fobject <- create_fairness_object( explainer_glm, explainer_ranger,
                                    data = compas,
                                    outcome = "Two_yr_Recidivism",
                                    base = "Caucasian",
                                    group = "Ethnicity")

  plt <- plot(group_metric(fobject))

  expect_equal(plt[[1]]$labels$title, "Group metric plot")
  expect_equal(plt[[1]]$labels$y, "ACC")

  # changing metrics

  gm <- group_metric(fobject, fairness_metric = "FPR", performance_metric = "f1")
  plt <- plot(gm)

  expect_equal(plt[[1]]$labels$y, "FPR")
  expect_equal(plt[[2]]$labels$y, "f1")


  print(gm)

})
