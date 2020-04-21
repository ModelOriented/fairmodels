
test_that("fairness objects metrics calculations", {


  data <- compas
  data$probabilities <- explainer_ranger$y_hat



  fobject10 <- create_fairness_object(explainer_ranger,
                                      data = data,
                                      outcome = "Two_yr_Recidivism",
                                      group = "Ethnicity",
                                      base = "Caucasian")

  data_C <- data[data$Ethnicity == "Caucasian",]
  preds01_C <- factor(round(data_C$probabilities))
  preds10_C <- relevel(preds01_C, "1")

  true01_C <- relevel(data_C$Two_yr_Recidivism, "0")
  true10_C <- relevel(data_C$Two_yr_Recidivism, "1")

  c01_C <- caret::confusionMatrix(preds01_C, true01_C, positive = "0")
  c10_C <- caret::confusionMatrix(preds10_C, true10_C)

  gm <- group_matrices(data,group = "Ethnicity", outcome = "Two_yr_Recidivism", outcome_numeric = explainer_ranger$y, cutoff = 0.5)

  expect_equal(gm$Caucasian$tp, c10_C$table[1,1])
  expect_equal(gm$Caucasian$fp, c10_C$table[1,2])
  expect_equal(gm$Caucasian$tn, c10_C$table[2,2])
  expect_equal(gm$Caucasian$fn, c10_C$table[2,1])

  table01_C <- c01_C$table
  table10_C <- c10_C$table

  data_A <- data[data$Ethnicity == "African_American",]
  preds01_A <- as.factor(round(data_A$probabilities))
  preds10_A <- relevel(preds01_A, "1")

  true10_A <- relevel(data_A$Two_yr_Recidivism, "1")
  c10_A <- caret::confusionMatrix(preds10_A, true10_A)

  table10_A <- c10_A$table

  expect_equal(gm$African_American$tp, table10_A[1,1])
  expect_equal(gm$African_American$fp, table10_A[1,2])
  expect_equal(gm$African_American$tn, table10_A[2,2])
  expect_equal(gm$African_American$fn, table10_A[2,1])

  # now checking if values in groups are scaled properly
  tpr10_C <- table10_C[1,1]/(table10_C[1,1] + table10_C[2,1])
  tpr10_A <- table10_A[1,1]/(table10_A[1,1] + table10_A[2,1])

  fobject_value <- round(fobject10$groups_data$ranger$TPR[3],3)
  names(fobject_value) <- NULL
  expect_equal(fobject_value, round(tpr10_A/tpr10_C,3))


})


