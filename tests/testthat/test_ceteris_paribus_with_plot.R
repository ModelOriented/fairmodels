test_that("test ceteris_paribus_cutoff with plot", {


  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American")

  expect_equal(cpc$subgroup, "African_American")
  expect_equal(cpc$cummulated, FALSE)

  metrics_used <- (unique(as.character(cpc$data$metric)))
  expect_equal(metrics_used,  unique_metrics())

  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American", cummulated = TRUE)
  # lm produces NA due to F1 parity loss

  expect_equal(cpc$cummulated, TRUE)

  ###################   plot    ###########################

  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American", cummulated = TRUE)

  plt <- plot(cpc)

  expect_equal(plt$labels$subtitle, "Based on African_American and cummulated" )
  expect_class(plt, "ggplot")

  cpc <- ceteris_paribus_cutoff(fobject, subgroup = "African_American")

  plt <- plot(cpc)
  expect_class(plt, "ggplot")
})
