test_that("test prints",{


  fh <- fairness_heatmap(fobject)
  expect_class(print(fh), "array")

  cm <- choose_metric(fobject)
  fo <- print(fobject)



})
