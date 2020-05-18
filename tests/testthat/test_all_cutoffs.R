test_that("Test all_cutoffs with plot", {

  ac <- all_cutoffs(fobject, explainer_label = "ranger", fairness_metrics = c("TPR_parity_loss", "ACC_parity_loss"))

  expect_equal(ac$explainer_label, "ranger")

  expect_equal(ac$data[ac$data$cutoff == 0.5,]$metric, c("TPR_parity_loss", "ACC_parity_loss" ))


  #################### plot #########################
  plt <- plot(ac)

  expect_equal(plt$labels$title, "All cutoffs plot")
  expect_equal(plt$labels$subtitle, "created with ranger")

})














