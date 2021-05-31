test_that("test ceteris_paribus_cutoff with plot", {


  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American")

  expect_equal(cpc$subgroup, "African_American")
  expect_equal(cpc$cumulated, FALSE)

  metrics_used <- (unique(as.character(cpc$cutoff_data$metric)))
  expect_equal(sort(metrics_used),  sort(fairness_check_metrics()))

  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American", cumulated = TRUE)
  # lm produces NA due to F1 parity loss

  expect_equal(cpc$cumulated, TRUE)

  ###################   plot    ###########################

  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American", cumulated = TRUE)

  plt <- plot(cpc)

  expect_equal(plt$labels$subtitle, "Based on African_American and cumulated" )
  expect_s3_class(plt, "ggplot")

  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American")

  plt <- plot(cpc)
  expect_s3_class(plt, "ggplot")
})
