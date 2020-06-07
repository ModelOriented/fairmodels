
test_that("test helper functions",{


  expect_equal(c("TPR_parity_loss", "TNR_parity_loss" ,"PPV_parity_loss", "NPV_parity_loss" ,"TS_parity_loss" , "ACC_parity_loss" ,"F1_parity_loss",  "MCC_parity_loss"),  unique_metrics())

  expect_error(assert_parity_metrics("non existing"))
  expect_error(assert_base_metrics("non existing"))
  expect_error(assert_performance_metrics("non existing"))

  df <- data.frame(a = c(1,0,1), b = c("e", NA, "v"), c = c(1,NA,0), d = c("a","b","c"))

  expect_warning(drop_metrics_with_na(df), "Found metric with NA: b, c, omiting it")
  expect_equal(drop_metrics_with_na(df), df[,c("a","d")])


})



