test_that("Test metric scores with plot", {

  ms <- metric_scores(fobject)

  for (i in seq_len(nrow(ms$metric_scores_data))){
    row <- ms$metric_scores_data[i,]
    expect_equal(fobject$groups_data[[row$model]][[row$metric]][[row$subgroup]], row$score)
  }

  expect_error(metric_scores(fobject, fairness_metrics = c("not in metrics")))
  expect_error(metric_scores("not that class"))

  ########## plot ###########

  plt <- plot(ms)
  expect_equal(plt$labels$title, "Metric scores plot")

})











