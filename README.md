  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/ModelOriented/FairModels/branch/master/graph/badge.svg)](https://codecov.io/gh/ModelOriented/FairModels?branch=master)
  [![R build status](https://github.com/ModelOriented/FairModels/workflows/R-CMD-check/badge.svg)](https://github.com/ModelOriented/FairModels/actions)
  <!-- badges: end -->
  
  

# FairModels 
## Overview

`FairModels` is package for fairness audit and visualization. Uses models explained with [DALEX](https://modeloriented.github.io/DALEX) and calculates fairness metrics based on confusion matrix for protected group.  Allows to compare and gain information about various machine learning models. *Make sure your models are classifying protected groups similarly*.



## Preview

![preview](man/figures/preview.gif)


### Used metrics

* **TPR** - True Positive Rate (Sensitivity, Recall)
* **TNR** - True Negative Rate (Specificity)
* **PPV** - Positive Predictive Value (Precision)
* **NPV** - Negative Predictive Value
* **FNR** - False Negative Rate
* **FPR** - False Positive Rate
* **FDR** - False Discovery Rate
* **FOR** - False Omision Rate
* **TS**  - Threat Score
* **ACC** - Accuracy
* **F1**  - F1 Score
* **MCC** - Matthews correlation coefficient

more on those metrics : [Confusion Matrix](https://en.wikipedia.org/wiki/Confusion_matrix)

### Installation

```
devtools::install_github("ModelOriented/fairmodels")
```

### Example

```
library(DALEX)
library(ranger)


# load data
data(compas)

# making classifiers
rf_compas <- ranger(Two_yr_Recidivism ~., data = compas, probability = TRUE)
lr_compas <- glm(Two_yr_Recidivism~., data=compas, family=binomial(link="logit"))
rf_compas_2 <- ranger(Two_yr_Recidivism ~ Age_Above_FourtyFive+Misdemeanor, data = compas, probability = TRUE)

# numeric target values
y_numeric <- as.numeric(compas$Two_yr_Recidivism)-1

# explaining with dalex
rf_explainer_1 <- explain(rf_compas, data = compas[,-1], y = y_numeric, label = "ranger")
lr_explainer_1 <- explain(lr_compas, data = compas[,-1], y = y_numeric, label = "logistic_regresion")
rf_explainer_2 <- explain(rf_compas_2, data = compas[,-1], y = y_numeric, label = "ranger2")


# creating fairness object
fobject <- create_fairness_object(rf_explainer_1, lr_explainer_1, rf_explainer_2,
                                  data = compas, 
                                  outcome = "Two_yr_Recidivism", 
                                  group = "Ethnicity",
                                  base = "Caucasian")

library(dplyr)

fobject %>% fairness_heatmap() %>% plot() 
fobject %>% fairness_radar() %>% plot() 
fobject %>% stack_metrics() %>% plot() 
fobject %>% group_metric() %>% plot() 
fobject %>% choose_metric() %>% plot() 
fobject %>% performance_and_fairness() %>% plot() 

```


#### Fairness object

`fairness object` consists of 
* x, ...  - explainer or list of explainers
* data    - full data (different explainers can be trained on different data)
* outcome - target variable
* group   - protected variable, usually race, sex, etc...
* base    - subgroup, base on which to calculate metrics. Metric on base subgroup is always 1. Usually specific race, sex etc...
* cutoff  - custom cutoff, might be single value - cutoff same for all subgroups or vector - for each subgroup individually


Tutorial: [Tutorial](https://modeloriented.github.io/FairModels/articles/Basic_tutorial.html)

#### Related works

Zafar,Valera, Rodriguez, Gummadi (2017)  https://arxiv.org/pdf/1610.08452.pdf
