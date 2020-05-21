test_that("Test choose_metric", {

  expect_equal(as.character(choose_metric(fobject)$metric), "FPR_parity_loss")

  expect_error(choose_metric(fobject, "I dont exist"))

  cm <- choose_metric(fobject, "TPR_parity_loss")
  cm_plot <- plot(cm)

  expect_class(cm_plot, "ggplot")


})
