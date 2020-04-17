library("DALEX")
library("ranger")
library("gbm")

data("compas")

y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1

glm_compas <-  glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
ranger_compas <- ranger(Two_yr_Recidivism~., data=compas, probability = TRUE)

df <- compas
df$Two_yr_Recidivism <- as.numeric(df$Two_yr_Recidivism) -1
gbm_compas <- gbm(Two_yr_Recidivism~., data=df)

explainer_glm    <- explain(glm_compas, data = compas[-1]  , y = y_numeric)
explainer_ranger <- explain(ranger_compas,data = compas[-1], y = y_numeric)
explainer_gbm <- explain(gbm_compas,data = compas[-1]   , y = y_numeric)

fobject_compas_proba <- create_fairness_object(explainer_glm,explainer_ranger, explainer_gbm,
                                               data = compas,
                                               outcome = "Two_yr_Recidivism",
                                               group = "Ethnicity",
                                               base = "Caucasian")




# testthat ----------------------------------------------------------------
error_message <- function(title, failed_values = NULL) paste0("Error! ", title, paste0(failed_values, collapse = ", "))
expect_class <- function(object, class) expect(any(base::class(object) %in% class), error_message(paste("object is", base::class(object), "not", class)))

