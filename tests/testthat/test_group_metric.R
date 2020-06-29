test_that("Test group_metrics ", {


  expect_error(group_metric(fobject, fairness_metric = 1231))
  expect_error(group_metric(fobject, fairness_metric = c("FPR", "TPR")))
  expect_error(group_metric(fobject, fairness_metric = "TPRR"))
  expect_error(group_metric(fobject, performance_metric = 1231))
  expect_error(group_metric(fobject, performance_metric = c("f1", "auc")))
  expect_error(group_metric(fobject, performance_metric = "f11"))


  fobject <- fairness_check( explainer_glm, explainer_ranger,
                             protected = compas$Ethnicity,
                             privileged = "Caucasian")

  plt <- plot(group_metric(fobject))

  expect_equal(plt[[1]]$labels$title, "Group metric plot")
  expect_equal(plt[[1]]$labels$y, "TPR")

  # changing metrics

  gm <- group_metric(fobject, fairness_metric = "FPR", performance_metric = "f1")
  plt <- plot(gm)

  expect_equal(plt[[1]]$labels$y, "FPR")
  expect_equal(plt[[2]]$labels$y, "f1")


  print(gm)

})
