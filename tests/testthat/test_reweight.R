test_that("Test Reweight classes", {

predicted_weights <- reweight(as.factor(c(1,1,1,1,1,0,0,0,0,0)), c(1,1,1,1,0,0,0,1,0,1))
# actual weights as in article
actual_weights    <- c(0.75,0.75,0.75,0.75,2, 0.67, 0.67, 1.5, 0.67, 1.5)

expect_equal(round(predicted_weights,2), actual_weights)

y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1

# numeric protected

expect_equal(reweight(compas$Sex, y_numeric),
             reweight(as.factor(compas$Sex), y_numeric))

expect_equal(reweight(compas$Sex, y_numeric),
             reweight(as.numeric(compas$Sex), y_numeric))


# difference in length
expect_error(reweight(compas$Sex[,-1], y_numeric))
# y must be 0 and 1
expect_error(reweight(compas$Sex[,-1], y_numeric + 1))


})

