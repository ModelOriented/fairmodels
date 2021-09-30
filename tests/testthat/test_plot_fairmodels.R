test_that("Test plot_fairmodels", {
  expect_s3_class(plot_fairmodels(explainer_gbm, protected = compas$Ethnicity, privileged = "Caucasian"), "ggplot")

  fc <- fobject_big

  suppressWarnings(expect_s3_class(plot_fairmodels(fc, type = "fairness_check"), "ggplot"))
  suppressWarnings(expect_s3_class(plot_fairmodels(fc, type = "stack_metrics"), "ggplot"))
  suppressWarnings(expect_s3_class(plot_fairmodels(fc, type = "fairness_heatmap"), "ggplot"))
  suppressWarnings(expect_s3_class(plot_fairmodels(fc, type = "fairness_pca"), "ggplot"))
  suppressWarnings(expect_s3_class(plot_fairmodels(fc, type = "fairness_radar", fairness_metrics = c("TPR", "TNR", "FPR", "ACC", "STP", "FOR", "PPV")), "ggplot"))
  expect_s3_class(plot_fairmodels(fc, type = "group_metric"), "ggplot")
  expect_s3_class(plot_fairmodels(fc, type = "choose_metric"), "ggplot")
  expect_s3_class(plot_fairmodels(fc, type = "metric_scores"), "ggplot")
  expect_s3_class(plot_fairmodels(fc, type = "performance_and_fairness"), "ggplot")
  expect_s3_class(plot_fairmodels(fc, type = "all_cutoffs"), "ggplot")
  expect_s3_class(plot_fairmodels(fc, type = "ceteris_paribus_cutoff", cumulated = TRUE, subgroup = "Caucasian"), "ggplot")

  expect_error(expect_error(plot_fairmodels(fc, type = "not_existing"), "ggplot"))
})
