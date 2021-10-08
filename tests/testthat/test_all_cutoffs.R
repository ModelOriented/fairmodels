test_that("Test all_cutoffs with plot", {
  ac <- all_cutoffs(fobject, fairness_metrics = c("TPR", "ACC"))

  tmp <- ac$cutoff_data[ac$cutoff_data$cutoff == 0.5, ]
  tmp <- tmp["metric"]
  tmp <- unique(as.character(unlist(tmp)))
  names(tmp) <- NULL
  expect_equal(tmp, c("TPR", "ACC"))


  #################### plot #########################
  plt <- plot(ac)

  expect_equal(plt$labels$title, "All cutoffs plot")

  plt <- plot(ac, label = "lm")
  expect_equal(plt$labels$subtitle, "created with lm")
})
