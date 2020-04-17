
test_that("PCA fairness", {

  n <- ncol(fobject_compas_proba$metric_data)
  f_pca <- create_fairness_pca(fobject_compas_proba)

  true_pca <- stats::prcomp(fobject_compas_proba$metric_data[,1:(n-1)], scale = TRUE)

  expect_equal(f_pca$x, true_pca$x)

  })




