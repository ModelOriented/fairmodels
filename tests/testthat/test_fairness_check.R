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

  ################################## plot #######################################

  plt <- plot(fobject)

  expect_equal(plt$labels$subtitle, "Created with lm, ranger")
  expect_equal(plt$labels$title , "Fairness check")
  expect_class(plt, "ggplot")

})
