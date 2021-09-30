test_that("Test heatmap", {
  small_plot <- plot(fairness_heatmap(fobject))[[3]]
  big_plot <- plot(fairness_heatmap(fobject_big), flip_axis = TRUE)[[3]]

  # heatmap converts shape while number of explainers exceed number of metrics
  expect_equal(c(small_plot$labels$x, small_plot$labels$y), c(big_plot$labels$y, big_plot$labels$x))

  top_dendogram <- plot(fairness_heatmap(fobject))[[1]]

  expect_equal(top_dendogram$labels$title, "Heatmap")
  expect_equal(top_dendogram$labels$subtitle, "With dendograms")
})
