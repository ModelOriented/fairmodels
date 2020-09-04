test_that("PCA fairness and plot", {

  n <- ncol(fobject$parity_loss_metric_data)
  data <- fobject$parity_loss_metric_data

  f_pca <- fairness_pca(fobject)
  data_c <- data[ , apply(data, 2, function(x) !any(is.na(x)))]
  data_c <- data_c[, colnames(data_c) %in% parity_loss_metrics()]
  true_pca <- stats::prcomp(data_c, scale = TRUE)

  expect_equal(f_pca$x, true_pca$x)

  f_pca2 <- fairness_pca(fobject, omit_models_with_NA = TRUE)
  data_r <- data[apply(data, 1, function(x) !any(is.na(x))), ]
  data_r <- data_r[, colnames(data_r) %in% parity_loss_metrics()]
  true_pca2 <- stats::prcomp(data_r, scale = TRUE)
  a <- as.data.frame(true_pca2$x)
  b <- as.data.frame(f_pca2$x)
  rownames(a) <- NULL
  rownames(b) <- NULL

  expect_equal(a,b )

  dummy_fobject <- fobject
  dummy_fobject$parity_loss_metric_data[2,3] <- NA

  expect_warning(fairness_pca(dummy_fobject), "Found metric with NA: PPV, omiting it")
  expect_warning(fairness_pca(dummy_fobject,omit_models_with_NA = TRUE ), "Found models with NA: lm, ommiting it")

  ########################################## PLOT #########################################

  fp <- suppressWarnings(fairness_pca(fobject_big))
  plt <- plot(fp)

  expect_class(plt, "ggplot")

  expect_equal(plt$labels$title, "Fairness PCA plot")


})





