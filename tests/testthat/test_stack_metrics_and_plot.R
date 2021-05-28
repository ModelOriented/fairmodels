test_that("test stack metric and plot",{

  sm <- stack_metrics(fobject)
  df <- expand_fairness_object(fobject)
  df$score <- round(df$score, 3)


  expect_equal(sm$stacked_data[sm$stacked_data$metric == "TPR" & sm$stacked_data$model == 'lm', 'score'],
               df[df$metric== "TPR" & df$model == 'lm', 'score'])


  plt <- plot(sm)

  expect_s3_class(plt, "ggplot")

  expect_equal(plt$labels$title, "Stacked Metric plot")

})
