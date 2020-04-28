test_that("Test demographic parity", {

  data(compas)

  expect_error(demographic_parity(compas, outcome = "Two_yr_Recidivism", group = "not existing"))
  expect_error(demographic_parity(compas, outcome = "not_existing", group = "Ethnicity"))

  plt1 <- demographic_parity(compas, outcome = "Two_yr_Recidivism", group =  "Ethnicity")
  plt2 <- demographic_parity(fobject)

  expect_equal(plt1$layers ,plt2$layers )
  expect_equal("Demographic parity plot", plt1$labels$title)

})



