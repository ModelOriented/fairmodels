test_that("Test fairness_check", {

  compas$Two_yr_Recidivism <- factor(compas$Two_yr_Recidivism, levels = c(1,0))

  rf_compas  <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
  glm_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))

  y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1

  explainer_rf  <- explain(rf_compas, data = compas, y = y_numeric)
  explainer_glm <- explain(glm_compas, data = compas, y = y_numeric)

  fobject <-create_fairness_object(explainer_glm, explainer_rf,
                                   outcome = "Two_yr_Recidivism",
                                   data  = compas,
                                   group = "Sex",
                                   base  = "Female",
                                   cutoff = 0.5)

  fc <- fairness_check(fobject)

  expect_class(fc, "fairness_check")

  metrics <- length(unique(as.character(fc$data$metric)))
  expect_equal(metrics, 5)

  expect_equal(as.character(fc$data$subgroup)[1], "Male")

  ################################## plot #######################################

  plt <- plot(fc)

  expect_equal(plt$labels$subtitle, "Created with lm, ranger")
  expect_equal(plt$labels$title , "Fairness check")
  expect_class(plt, "ggplot")

})
