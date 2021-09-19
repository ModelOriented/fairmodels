test_that("Test fairness_check", {

  compas$Two_yr_Recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))

  rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
  glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))

  y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1

  explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
  explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)

  fobject <- fairness_check(explainer_glm, explainer_rf,
                                   protected = compas$Sex,
                                   privileged = "Female",
                                   cutoff = 0.5)


  fc_data <- fobject$fairness_check_data

  metrics <- length(unique(as.character(fc_data$metric)))
  expect_equal(metrics, 5)

  expect_equal(as.character(fc_data$subgroup)[1], "Male")

  expect_equal(fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = 0.5,
                             verbose = FALSE),
               fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = 0.5,
                              colorize = FALSE))

  expect_equal(fairness_check(explainer_glm, explainer_rf,
                              protected = as.numeric(compas$Sex)-1,
                              privileged = 0,
                              cutoff = 0.5,
                              verbose = FALSE),
               fairness_check(explainer_glm, explainer_rf,
                              protected = as.numeric(compas$Sex)-1,
                              privileged = "0",
                              cutoff = 0.5,
                              colorize = FALSE))



  # errors

  expect_error(fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = list(femalle = 0.5)))

  expect_error(fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = list(female = 0.5, female = 0.4)))

  expect_error(fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = list(femalle = "not_numeric")))

  expect_error(fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = list(female = 123)))


  expect_error(fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = list(female = 0.5),
                              epsilon = 3))

  expect_error(fairness_check(explainer_glm, explainer_rf,
                              protected = compas$Sex,
                              privileged = "Female",
                              cutoff = list(female = 0.5),
                              epsilon = c(0.3, 0.5)))

  fobject2 <- fobject
  expect_error(fairness_check(fobject, fobject2))


  exp2 <- explainer_glm
  exp2$model_info$type <- 'regression'
  expect_error(fairness_check(exp2, fobject))


  fobject2$protected <- compas$Sex[1:6000]
  suppressWarnings( expect_error(fairness_check(fobject, fobject2)))
  suppressWarnings( expect_error(fairness_check(fobject, fobject2, privileged = "Female")))
  suppressWarnings(expect_error(fairness_check(fobject, fobject2, privileged = "Female", protected = compas$Sex)))

  # good calculations
  calculated_val_TPR <- fobject$fairness_check_data[fobject$fairness_check_data$model == "lm" & fobject$fairness_check_data$metric == 'Equal opportunity ratio     TP/(TP + FN)', "score" ]
  calculated_val_FPR <- fobject$fairness_check_data[fobject$fairness_check_data$model == "lm" & fobject$fairness_check_data$metric == 'Predictive equality ratio   FP/(FP + TN)', "score" ]
  calculated_val_PPV <- fobject$fairness_check_data[fobject$fairness_check_data$model == "lm" & fobject$fairness_check_data$metric == 'Predictive parity ratio     TP/(TP + FP)', "score" ]
  calculated_val_STP <- fobject$fairness_check_data[fobject$fairness_check_data$model == "lm" & fobject$fairness_check_data$metric == 'Statistical parity ratio   (TP + FP)/(TP + FP + TN + FN)', "score" ]
  calculated_val_ACC <- fobject$fairness_check_data[fobject$fairness_check_data$model == "lm" & fobject$fairness_check_data$metric == 'Accuracy equality ratio    (TP + TN)/(TP + FP + TN + FN)', "score" ]

  actual_val_TPR <- fobject$groups_data$lm$TPR[2]/fobject$groups_data$lm$TPR[1]
  actual_val_FPR <- fobject$groups_data$lm$FPR[2]/fobject$groups_data$lm$FPR[1]
  actual_val_PPV <- fobject$groups_data$lm$PPV[2]/fobject$groups_data$lm$PPV[1]
  actual_val_STP <- fobject$groups_data$lm$STP[2]/fobject$groups_data$lm$STP[1]
  actual_val_ACC <- fobject$groups_data$lm$ACC[2]/fobject$groups_data$lm$ACC[1]


  names(actual_val_TPR) <- NULL
  names(actual_val_FPR) <- NULL
  names(actual_val_PPV) <- NULL
  names(actual_val_STP) <- NULL
  names(actual_val_ACC) <- NULL

  expect_equal(calculated_val_TPR , actual_val_TPR)
  expect_equal(calculated_val_FPR , actual_val_FPR)
  expect_equal(calculated_val_PPV , actual_val_PPV)
  expect_equal(calculated_val_STP , actual_val_STP)
  expect_equal(calculated_val_ACC , actual_val_ACC)


  ################################## plot #######################################

  plt <- plot(fobject)

  expect_equal(plt$labels$subtitle, "Created with lm, ranger")
  expect_equal(plt$labels$title , "Fairness check")
  expect_s3_class(plt, "ggplot")

})
