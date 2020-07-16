
test_that("test helper functions",{


  expect_equal(c("TPR", "TNR" ,"PPV", "NPV",
                 "TS" , "STP", "ACC" ,"F1",
                 "MCC"),  unique_metrics())

  expect_error(assert_parity_metrics("non existing"))
  expect_error(assert_base_metrics("non existing"))
  expect_error(assert_performance_metrics("non existing"))

  df <- data.frame(a = c(1,0,1), b = c("e", NA, "v"), c = c(1,NA,0), d = c("a","b","c"))

  expect_warning(drop_metrics_with_na(df), "Found metric with NA: b, c, omiting it")
  to_compare <- suppressWarnings(drop_metrics_with_na(df))
  expect_equal(to_compare, df[,c("a","d")])

  # colors may change
  expect_class(colors_fairmodels(7), 'character')
  expect_class(colors_fairmodels(8), 'character')
  expect_class(colors_fairmodels(9), 'character')
  expect_class(colors_fairmodels(10), 'character')
  expect_class(colors_fairmodels(11), 'character')
  expect_class(colors_fairmodels(12), 'character')
  expect_class(colors_fairmodels(13), 'character')
  expect_class(colors_fairmodels(100), 'character')

})



