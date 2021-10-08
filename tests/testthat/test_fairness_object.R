
test_that("test fairness object", {
  data <- compas
  data$`_probabilities_` <- explainer_ranger$y_hat



  fobject10 <- fairness_check(explainer_ranger,
    protected  = compas$Ethnicity,
    privileged = "Caucasian"
  )

  data_C <- data[data$Ethnicity == "Caucasian", ]
  preds01_C <- factor(round(data_C$`_probabilities_`))
  preds10_C <- relevel(preds01_C, "1")

  true01_C <- relevel(data_C$Two_yr_Recidivism, "0")
  true10_C <- relevel(data_C$Two_yr_Recidivism, "1")

  tp_C <- sum(true10_C == preds10_C & true10_C == 1)
  tn_C <- sum(true10_C == preds10_C & true10_C == 0)
  fp_C <- sum(true10_C != preds10_C & true10_C == 0)
  fn_C <- sum(true10_C != preds10_C & true10_C == 1)


  custom_cutoff <- as.list(rep(0.5, 6))
  names(custom_cutoff) <- levels(compas$Ethnicity)
  gm <- group_matrices(
    protected = compas$Ethnicity,
    probs = explainer_ranger$y_hat,
    preds = explainer_ranger$y,
    cutoff = custom_cutoff
  )

  expect_equal(gm$Caucasian$tp, tp_C)
  expect_equal(gm$Caucasian$fp, fp_C)
  expect_equal(gm$Caucasian$tn, tn_C)
  expect_equal(gm$Caucasian$fn, fn_C)


  data_A <- data[data$Ethnicity == "African_American", ]
  preds01_A <- as.factor(round(data_A$`_probabilities_`))
  preds10_A <- relevel(preds01_A, "1")

  true10_A <- relevel(data_A$Two_yr_Recidivism, "1")

  tp_A <- sum(true10_A == preds10_A & true10_A == 1)
  tn_A <- sum(true10_A == preds10_A & true10_A == 0)
  fp_A <- sum(true10_A != preds10_A & true10_A == 0)
  fn_A <- sum(true10_A != preds10_A & true10_A == 1)


  expect_equal(gm$African_American$tp, tp_A)
  expect_equal(gm$African_American$fp, fp_A)
  expect_equal(gm$African_American$tn, tn_A)
  expect_equal(gm$African_American$fn, fn_A)

  # now checking if values in groups are scaled properly
  tpr_C <- tp_C / (tp_C + fn_C)
  tpr_A <- tp_A / (tp_A + fn_A)

  fobject_value <- round(fobject10$groups_data$ranger$TPR["Caucasian"], 3)
  names(fobject_value) <- NULL
  expect_equal(fobject_value, round(tpr_C, 3))

  fobject_value <- round(fobject10$groups_data$ranger$TPR["African_American"], 3)
  names(fobject_value) <- NULL
  expect_equal(fobject_value, round(tpr_A, 3))

  # parity loss

  actual_val <- fobject$parity_loss_metric_data$TPR[1]
  calculated_val <- sum(abs(log(fobject$groups_data$gbm$TPR / fobject$groups_data$gbm$TPR["Caucasian"])))

  expect_equal(actual_val, calculated_val)

  # no such level
  expect_error(fairness_check(explainer_ranger,
    protected  = compas$Ethnicity,
    privileged = "notexisting"
  ))

  # different lenght
  expect_error(fairness_check(explainer_ranger,
    protected  = compas$Ethnicity[1:(length(compas$Ethnicity) - 1)],
    privileged = "Caucasian"
  ))

  fc <- fairness_check(explainer_ranger,
    protected  = compas$Ethnicity,
    privileged = "Caucasian"
  )

  # same labels
  expect_error(fairness_check(explainer_ranger, fc,
    protected  = compas$Ethnicity,
    privileged = "Caucasian"
  ))

  new_exp <- explainer_ranger
  new_exp$y[4] <- 1
  new_exp$label <- "error"

  new_vec <- compas$Ethnicity
  new_vec[3] <- "Other"

  fc2 <- fairness_check(new_exp,
    protected  = new_vec,
    privileged = "Caucasian"
  )


  # incompatible fairness objects
  expect_error(fairness_check(fc, fc2,
    protected  = compas$Ethnicity,
    privileged = "Caucasian"
  ))

  # different y in explainers
  expect_error(fairness_check(new_exp, explainer_ranger,
    protected  = compas$Ethnicity,
    privileged = "Caucasian"
  ))


  expect_error(fairness_check(explainer_ranger,
    protected = compas$Ethnicity,
    privileged = "Caucasian",
    cutoff = 1.3
  ))

  expect_error(fairness_check(explainer_ranger,
    protected = compas$Ethnicity,
    privileged = "Caucasian",
    cutoff = c(1, 0.3)
  ))

  # checking if fobjects can be put in fairness_check
  fc3 <- fairness_check(explainer_ranger,
    protected = compas$Ethnicity,
    privileged = "Caucasian",
    label = "second_fobject"
  )

  fc4 <- fairness_check(fc3, fc)


  ######################## plot #############################

  plt <- plot(fc)

  expect_s3_class(plt, "ggplot")
})
