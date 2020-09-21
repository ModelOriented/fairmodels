# fairmodels 0.2.2
* `fairness_check_data` now instead of `0` has `NA` due to concerns of interpretability - insignificant difference could lead up to maximal value of loss. With that change when `NA` is created user will see warning when plotting or printing. This doesn't affect other objects and plots.
* Description fixes
* Added `metric_scores` plot to basic tutorial
* Updated new documentation in `roc_pivot`

# fairmodels 0.2.1
* bug related to `fairness check plot` fixed - rectangles did not appear for low epsilon values

# fairmodels 0.2.0
* adhering to four-fifths (80%) rule - changed fairness check and parity loss calculation. Now ratio is being calculated instead of differences.(#17)
* Some plots now have default fairness metrics - same as in `fairness_check` 
* `stack_metrics` now has parameter `fairness_metrics`
* corrected vignettes
* enhanced tests

# fairmodels 0.1.1
* changed examples in `metric_scores` function
* changed `DALEX` URL in README
* changed pre-processing to preprocessing in DESCRIPTION

# fairmodels 0.1.0

* main function `fairness_check()` implemented 
* various bias visualization functions implemented 
* pre-processing and post-processing bias mitigation techniques implemented
* 2 vignettes present
