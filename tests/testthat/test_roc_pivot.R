test_that("test ROC pivot", {

  df <- data.frame(sex = as.factor(c(rep("M",5),rep("F",5),rep("N",5))),
                   target = c(1,1,1,1,0,0,0,1,0,1, 0,0,0,0,1),
                   name = as.character(1:15),
                   probs = c(0.9, 0.82, 0.54, 0.78, 0.455, 0.12, 0.48,0.63,0.48, 0.88, 0.34, 0.12, 0.34, 0.49, 0.9 ))

  lr_model <- glm(target ~ sex + name, data = df, family = binomial())

  # explainer will have no y_hats close to border - no difference
  explainer <- suppressWarnings(DALEX::explain(lr_model, data = df[,c("sex","name")], y = df$target))
  explainer2 <- roc_pivot(explainer, protected = df$sex, "M")

  expect_equal(explainer$y_hat, explainer2$y_hat)

  explainer$y_hat <- df$probs
  explainer2 <- roc_pivot(explainer, protected = df$sex, "M", theta = 0.05)

  # changed probs
  expect_equal(explainer2$y_hat, c(0.9, 0.82,0.46,0.78,0.455,0.12,0.52,0.63,0.52,0.88,0.34,0.12,0.34,0.51,0.9))

  # errors
  explainer2$model_info$type <- "not_classification"
  expect_error(roc_pivot(explainer2, protected = df$sex, "M", theta = 0.05))
  expect_output(roc_pivot(explainer, protected = as.character(df$sex), "M", theta = 0.05))
  expect_error(roc_pivot(explainer, protected = df$sex, "M", theta = 0.05, cutoff = 2))
  expect_error(roc_pivot(explainer, protected = df$sex, "M", theta = 0.05, cutoff = c(0.1, 0.5)))
  suppressWarnings(expect_error(roc_pivot(explainer, protected = df$sex, "M", theta = 0.05, cutoff = 0.5, theta = 3)))
  expect_error(roc_pivot(explainer, protected = df$sex, "M", theta = 0.05, cutoff = 0.5, theta = c(0.3, 0.1)))

})
