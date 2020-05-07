test_that("Test_fairness_radar_and_plot", {

  fradar <- fairness_radar(fobject)
  n <- ncol(fobject$metric_data) -1

  expect_equal(fradar$n, length(fobject$explainers))

  metrics <- fobject$metric_data[,1:n]
  models  <- fobject$metric_data[,n+1]

  for (metric in unique_metrics()){
    for (model in models){
      actual <- fobject$metric_data[fobject$metric_data$label == model, metric]
      to_check <- as.character(fradar$df$metric) == metric & as.character(fradar$df$model) == model
      expect_equal(fradar$df[to_check,"score"], actual)
    }
  }

  plt <- plot(fradar)

  # checking if plot data is equal to df scaled by max val
  expect_equal(plt$data$score, fradar$df$score/max(fradar$df$score))

  expect_class(plt, "ggplot")

})
