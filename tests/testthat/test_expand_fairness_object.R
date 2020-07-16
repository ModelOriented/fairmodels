test_that("expand_fairness_object values check", {

  efo               <- expand_fairness_object(fobject)
  parity_loss_metric_data       <- fobject$parity_loss_metric_data
  parity_loss_metric_data$label <- fobject$label

  metrics <- as.character(unique(efo$metric))
  models  <- as.character(unique(efo$model))

  # for each value check if corresponds to equal value in metrics_data
  for (model in models){
    for (metric in metrics){
      expect_equal(efo[efo$metric == metric & efo$model == model, "score"],
                   parity_loss_metric_data[parity_loss_metric_data$label == model, metric])
    }
  }

})





