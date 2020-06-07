test_that("Test class parity", {

  data(compas)

  expect_error(class_parity(compas, outcome = "Two_yr_Recidivism", group = "not existing"))
  expect_error(class_parity(compas, outcome = "not_existing", group = "Ethnicity"))

  plt1 <- class_parity(compas, outcome = "Two_yr_Recidivism", group =  "Ethnicity")
  plt2 <- class_parity(fobject)

  expect_equal(plt1$layers ,plt2$layers )
  expect_equal("Class parity plot", plt1$labels$title)

})



