test_that("expand_fairness_object values check", {

  efo               <- expand_fairness_object(fobject)
  metric_data       <- fobject$metric_data
  metric_data$label <- fobject$labels

  metrics <- as.character(unique(efo$metric))
  models  <- as.character(unique(efo$model))

  # for each value check if coresponds to equal value in metrisc_data
  for (model in models){
    for (metric in metrics){
      expect_equal(efo[efo$metric == metric & efo$model == model, "score"],
                   metric_data[metric_data$label == model, metric])
    }
  }

})





