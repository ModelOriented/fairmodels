# Instruction for adding one custom metric to fairmodels.

1. Make a fork of the repository 
2. In file `R/calculate_group_fairness_metrics.R` change number of columns to 13 in "group_metric_matrix". Add name of metric to "rownames(group_metric_matrix)". Add your metric by defining it with fields from confusion matrix (tp, tn, fp, fn). Add it to "group_metric_matrix".
3. In file `R/fairness_check.R` in place where "parity_loss_metric_data" is defined, change number of columns to 13. 
4. In file `R/heper_functions.R` add your metrics name to "parity_loss_metrics"

That is all. Now when creating fairness object your new metric will be visible in fileds `parity_loss_metric_data` and `groups_data`. To visualize your metric, just add its name to parameter responsible for fairness metric, like in here: 
```stack_metrics(fobject, fairness_metrics = c("ACC", "TPR", "PPV", "FPR", "STP", "NEW_METRIC")) %>% plot()```
