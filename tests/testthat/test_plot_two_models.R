test_that("Test plot two models ", {


  expect_error(plot_two_models(fobject))

  fobject <- create_fairness_object( explainer_glm, explainer_ranger,
                                    data = compas,
                                    outcome = "Two_yr_Recidivism",
                                    base = "Caucasian",
                                    group = "Ethnicity")

  plt <- plot_two_models(fobject)

  expect_equal(plt[[1]]$labels$title, "2 models plot")
  expect_equal(plt[[1]]$labels$subtitle, "Created with lm and ranger")
  expect_equal(plt[[1]]$labels$y, "ACC")

  # changing metrics
  plt <- plot_two_models(fobject, fairness_metric = "FPR", performance_metric = "f1")

  expect_equal(plt[[1]]$labels$y, "FPR")
  expect_equal(plt[[2]]$labels$y, "f1")


})
