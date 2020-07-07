library("DALEX")
library("ranger")
library("gbm")

data("compas")



y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1

glm_compas    <-  glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
ranger_compas <- ranger::ranger(Two_yr_Recidivism~., data=compas, probability = TRUE)

df <- compas
df$Two_yr_Recidivism <- as.numeric(df$Two_yr_Recidivism) -1
gbm_compas <- gbm::gbm(Two_yr_Recidivism~., data=df , distribution = "bernoulli")

explainer_glm    <- DALEX::explain(glm_compas, data = compas[-1]  , y = y_numeric, verbose = FALSE)
explainer_ranger <- DALEX::explain(ranger_compas,data = compas[-1], y = y_numeric, verbose = FALSE)
explainer_gbm    <- DALEX::explain(gbm_compas,data = compas[-1]   , y = y_numeric, verbose = FALSE)

fobject <- fairness_check(explainer_gbm, explainer_glm, explainer_ranger,
                                  protected = compas$Ethnicity,
                                  privileged = "Caucasian",
                                  cutoff = list(Asian = 0.45),
                                  verbose = FALSE)



explainer_gbm1 <- explainer_gbm
explainer_gbm2 <- explainer_gbm
explainer_gbm3 <- explainer_gbm
explainer_gbm4 <- explainer_gbm

explainer_gbm1$label <- "gmb1"
explainer_gbm2$label <- "gmb2"
explainer_gbm3$label <- "gmb3"
explainer_gbm4$label <- "gmb4"

explainer_ranger1 <- explainer_ranger
explainer_ranger2 <- explainer_ranger
explainer_ranger3 <- explainer_ranger
explainer_ranger4 <- explainer_ranger

explainer_ranger1$label <- "ranger1"
explainer_ranger2$label <- "ranger2"
explainer_ranger3$label <- "ranger3"
explainer_ranger4$label <- "ranger4"

explainer_glm1 <- explainer_glm
explainer_glm2 <- explainer_glm

explainer_glm1$label <- "glm1"
explainer_glm2$label <- "glm2"

fobject_big <- fairness_check(explainer_gbm, explainer_glm, explainer_ranger,
                                      explainer_gbm1, explainer_glm1, explainer_ranger1,
                                      explainer_gbm2, explainer_glm2, explainer_ranger2,
                                      explainer_gbm3, explainer_ranger3,
                                      explainer_gbm4, explainer_ranger4,
                              protected = compas$Ethnicity,
                              privileged = "Caucasian",
                              cutoff = list(African_American = 0.4),
                              verbose = FALSE)

# testthat ----------------------------------------------------------------
error_message <- function(title, failed_values = NULL) paste0("Error! ", title, paste0(failed_values, collapse = ", "))
expect_class <- function(object, class) expect(any(base::class(object) %in% class), error_message(paste("object is", base::class(object), "not", class)))


