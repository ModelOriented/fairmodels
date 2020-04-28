test_that("Test heatmap",{

  small_plot <- plot_heatmap(fobject)[[3]]
  big_plot   <- plot_heatmap(fobject_big)[[3]]

  # heatmap converts shape while number of explainers exceed number of metrics
  expect_equal(c(small_plot$labels$x,small_plot$labels$y),c(big_plot$labels$y,big_plot$labels$x) )


  # scaling
  plt <- plot_heatmap(fobject, scale = TRUE)[[3]]

  expected_val_hat <- mean(plt$data[plt$data$metric == "TPR_parity_loss",]$score)
  S_hat     <- (sd(plt$data[plt$data$metric == "TPR_parity_loss",]$score))^2

  expect_lt(abs(expected_val_hat) , 0.001)
  expect_lt(abs(S_hat-1) , 0.001)



})
