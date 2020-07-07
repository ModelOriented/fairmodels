test_that("confusion_matrices_for_groups", {

  test_data <- data.frame(sex = as.factor(c(rep( "male", 7 ), rep("female", 7))),
                          target        = c( 1,    1,   1,  0,  0,  0,   0,      1,   1,  1,  1,  0,   0,   0),
                          probabilities = c(0.61,0.55,0.1,0.1,0.45,0.69,0.88,   0.4,0.9,0.8,0.13,0.98,0.1,0.7))
  test_data$sex <- relevel(test_data$sex, "male")
  # should give numeric true_level
  expect_error(group_matrices(protected = test_data$sex,
                              preds = as.factor(test_data$target),
                              probs = test_data$probabilities))
  gm <- group_matrices(protected = test_data$sex,
                       preds = test_data$target,
                       probs = test_data$probabilities,
                       cutoff = list(male = 0.6, female = 0.4))

  expect_equal(gm$female$tp, 3)
  expect_equal(gm$female$tn, 1)
  expect_equal(gm$female$fp, 2)
  expect_equal(gm$female$fn, 1)
  expect_equal(gm$male$tn, 2)
  expect_equal(gm$male$tp, 1)
  expect_equal(gm$male$fp, 2)
  expect_equal(gm$male$fn, 2)



})
