test_that("Test_fairness_radar_and_plot", {

  fradar <- fairness_radar(fobject)

  metrics <- fobject$parity_loss_metric_data
  models  <- fobject$labels

  for (metric in fairness_check_metrics()){
    for (model in models){
      actual <- fobject$parity_loss_metric_data[fobject$labels == model, metric]
      to_check <- as.character(fradar$data$metric) == metric & as.character(fradar$data$model) == model
      expect_equal(fradar$data[to_check,"score"], actual)
    }
  }

  expect_error(fairness_radar(fobject, fairness_metrics = 1))
  fo <- fobject
  fo$parity_loss_metric_data[2,1] <- NA

  expect_warning(fairness_radar(fo))

  fo$parity_loss_metric_data[2,1:11] <- NA

  # both warning and error
  expect_warning(expect_error(fairness_radar(fo)))

  ############### plot #######################
  plt       <- plot(fradar)
  crd_radar <- coord_radar()

  # checking if plot data is equal to data scaled by max val
  expect_equal(plt$data$score, fradar$radar_data$score/max(fradar$radar_data$score))
  expect_s3_class(crd_radar, "CordRadar")

  expect_s3_class(plt, "ggplot")

  ggproto("CordRadar", CoordPolar, theta = "x", r = "y", start = - pi / 3,
          direction = 1, is_linear = function() TRUE, render_bg = render_bg_function)

  expect_error(render_bg_function())
  expect_error(theta_rescale())

})
