test_that("test resample", {

  df <- data.frame(sex = as.factor(c(rep("M",5),rep("F",5),rep("N",5))),
                   target = c(1,1,1,1,0,0,0,1,0,1, 0,0,0,0,1))

  MN <- sum(df$sex == "M" & df$target == 0)
  MP <- sum(df$sex == "M" & df$target == 1)
  FN <- sum(df$sex == "F" & df$target == 0)
  FP <- sum(df$sex == "F" & df$target == 1)
  NN <- sum(df$sex == "N" & df$target == 0)
  NP <- sum(df$sex == "N" & df$target == 1)

  weights <- reweight(df$sex, df$target)

  wMP <- weights[1]
  wMN <- weights[5]
  wFP <- weights[10]
  wFN <- weights[6]
  wNN <- weights[13]
  wNP <- weights[15]

  # expected
  E_MP <- round(MP * wMP)
  E_MN <- round(MN * wMN)
  E_FN <- round(FN * wFN)
  E_FP <- round(FP * wFP)
  E_NP <- round(NP * wNP)
  E_NN <- round(NN * wNN)

  # uniform
  df_2 <- df[resample(df$sex, df$target),]

  MN_2 <- sum(df_2$sex == "M" & df_2$target == 0)
  MP_2 <- sum(df_2$sex == "M" & df_2$target == 1)
  FN_2 <- sum(df_2$sex == "F" & df_2$target == 0)
  FP_2 <- sum(df_2$sex == "F" & df_2$target == 1)
  NN_2 <- sum(df_2$sex == "N" & df_2$target == 0)
  NP_2 <- sum(df_2$sex == "N" & df_2$target == 1)

  expect_equal(E_MP, MP_2)
  expect_equal(E_MN, MN_2)
  expect_equal(E_FP, FP_2)
  expect_equal(E_FN, FN_2)
  expect_equal(E_NP, MP_2)
  expect_equal(E_NN, NN_2)

  # preferential
  df <- data.frame(sex = as.factor(c(rep("M",5),rep("F",5),rep("N",5))),
                   target = c(1,1,1,1,0,0,0,1,0,1, 0,0,0,0,1),
                   name = as.character(1:15),
                   probs = c(0.9, 0.82, 0.56, 0.78, 0.45, 0.12, 0.48,0.63,0.48, 0.88, 0.34, 0.12, 0.34, 0.49, 0.9 ),
                   stringsAsFactors = FALSE)

  df_3 <- df[resample(df$sex, df$target, type = "preferential", probs = df$probs),]

  expect_equal(sort(as.numeric(df_3$name)), c(1,2,5,5,5,6,7,8,9,10,11,12,13,15,15))

  expect_error(resample(df$sex, df$target, type = "preferential", probs = df$probs, cutoff = 12))
  expect_error(resample(df$sex, df$target, type = "preferential", probs = df$probs, cutoff = c(0.3, 0.4)))



})
