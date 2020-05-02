test_that("PCA fairness", {

  n <- ncol(fobject$metric_data)
  data <- fobject$metric_data[,1:(n-1)]

  f_pca <- pca(fobject)
  data_c <- data[ , apply(data, 2, function(x) !any(is.na(x)))]
  true_pca <- stats::prcomp(data_c, scale = TRUE)

  expect_equal(f_pca$x, true_pca$x)

  f_pca2 <- pca(fobject, omit_models_with_NA = TRUE)
  data_r <- data[apply(data, 1, function(x) !any(is.na(x))), ]
  true_pca2 <- stats::prcomp(data_r, scale = TRUE)

  expect_equal(f_pca2$x, true_pca2$x)

})
