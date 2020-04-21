  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/jakwisn/FairModels/branch/master/graph/badge.svg)](https://codecov.io/gh/jakwisn/FairModels?branch=master)

  [![R build status](https://github.com/jakwisn/FairModels/workflows/R-CMD-check/badge.svg)](https://github.com/jakwisn/FairModels/actions)
  <!-- badges: end -->
  
  

# FairModels 
## Overwiew

`FairModels` is package for fairness audit and visualization. Uses models explained with [DALEX](https://modeloriented.github.io/DALEX) and calculates fairness metrics based on confusion matrix for protected group.  Allows to compare and gain information about various machine learning models. *Make sure your models are classifying protected groups similarly*.

## Preview

<center>
<img src="man/figures/heatmap.png">
</center>

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
`

