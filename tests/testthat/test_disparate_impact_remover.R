test_that("test disparate impact remover", {

  df <- data.frame(kind = as.factor(c(rep("Blue", 100), rep("Green", 100), rep("Violet", 100))),
                   score = c(rnorm(100, 140, 40), rnorm(100, 200, 50), rnorm(100, 175, 30)))


  df2 <- disparate_impact_remover(df, protected = df$kind, features_to_transform = "score")

  mb <- mean(df2[df$kind == "Blue","score"])
  mg <- mean(df2[df$kind == "Green","score"])
  mv <- mean(df2[df$kind == "Violet","score"])

  expect_equal(mb, mg)
  expect_equal(mb, mv)

  expect_error(disparate_impact_remover(df, protected = as.numeric(df$kind), features_to_transform = "score"))
  expect_error(disparate_impact_remover(df, protected = df$kind, features_to_transform = "scoreeee"))
  expect_error(disparate_impact_remover(df, protected = df$kind, features_to_transform = "kind"))



})










