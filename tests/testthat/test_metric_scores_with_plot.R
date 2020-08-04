test_that("Test metric scores with plot", {

  ms <- metric_scores(fobject)

  # check if expanded correctly
  for (model in fobject$label){
    for (metric in unique(ms$metric_scores_data$metric)){
      for (subgroup in unique((ms$metric_scores_data$subgroup))){
        value <- fobject$groups_data[[model]][[metric]][subgroup]
        names(value) <- NULL
        expect_equal(value, ms$metric_scores_data[ms$metric_scores_data$subgroup == subgroup & ms$metric_scores_data$metric == metric & ms$metric_scores_data$model == model, "score"])
      }
    }
  }

  expect_error(metric_scores(ms))
  expect_error(metric_scores(fobject,fairness_metrics = c("not in metrics")))

  plt <- plot(ms)
  expect_class(plt, "ggplot")

})











