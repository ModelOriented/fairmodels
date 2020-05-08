test_that("test prints",{


  fh <- fairness_heatmap(fobject)
  expect_class(print(fh), "matrix")

  cm <- choose_metric(fobject)
  fo <- print(fobject)



})
