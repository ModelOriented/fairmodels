test_that("Test all_cutoffs with plot", {

  ac <- all_cutoffs(fobject, label = "ranger", fairness_metrics = c("TPR_parity_loss", "ACC_parity_loss"))

  expect_equal(ac$label, "ranger")

  tmp <- ac$cutoff_data[ac$cutoff_data$cutoff == 0.5,]
  tmp <- tmp["metric"]
  tmp <- as.character(unlist(tmp))
  names(tmp) <- NULL
  expect_equal(tmp, c("TPR_parity_loss", "ACC_parity_loss" ))


  #################### plot #########################
  plt <- plot(ac)

  expect_equal(plt$labels$title, "All cutoffs plot")
  expect_equal(plt$labels$subtitle, "created with ranger")

})














