test_that("test stack metric and plot",{

  sm <- stack_metrics(fobject)
  df <- expand_fairness_object(fobject)
  df$score <- round(df$score, 3)

  expect_equal(sm$expanded_data,  df[df$metric %in% unique_metrics(),] )


  plt <- plot(sm)

  expect_class(plt, "ggplot")

  expect_equal(plt$labels$fill, "Metric")
  expect_equal(plt$labels$title, "Stacked Metric Chart")
  expect_equal(plt$labels$x, "Model Label")
  expect_equal(plt$labels$y, "Cummulated metric score")




})
