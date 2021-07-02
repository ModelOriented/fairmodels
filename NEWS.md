# fairmodels 1.1.1
* Fixed error which appeared when 2 fairness objects had the same labels in them. Now if this appears it throws an error. [(#41)](https://github.com/ModelOriented/fairmodels/issues/41)
* `privileged` parameter is now converted to character. [(#41)](https://github.com/ModelOriented/fairmodels/issues/41)
* `reweight()` function now accepts factors [(#41)](https://github.com/ModelOriented/fairmodels/issues/41)

# fairmodels 1.1.0
* Added function `fairness_check_regression()` that supports regression models along with 2 plot types [(#38)](https://github.com/ModelOriented/fairmodels/issues/38).
* Added additional tests. 
* Modularized `fairness_check()` code.
* Changed x-axis ticks generation in `fairness_check()`. 
* Fixed issues with `plot_density
* Updated links in README and DESCRIPTION.

# fairmodels 1.0.1 
* Changed examples - added parameter `num.threads = 1` to `ranger` and added *donttest{}* to examples with long computation time. 

# fairmodels 1.0.0
* Added citation information
* Added additional reference in `fairness_check()` documentation.
* Fixed links in DESCRIPTION and README. 

# fairmodels 0.2.6 
* Fixed bug which appeared when two fairness objects were passed to `fairness_check` without an explainer. [(#36)](https://github.com/ModelOriented/fairmodels/issues/36)

# fairmodels 0.2.5
* Extended documentation for `epsilon` parameter in `fairness_check()` function.

# fairmodels 0.2.4
* Deleted on-load information message about four-fifths rule. 
* Fixed bug with `NA` warning in metrics that are not chosen. [(#32)](https://github.com/ModelOriented/fairmodels/issues/32)

# fairmodels 0.2.3
* Fixed the way the `parity_loss` is calculated in `all_cutoffs` and `ceteris_paribus_cutoff`. [(#24)](https://github.com/ModelOriented/fairmodels/issues/24)
* Updated vignettes
* changed documentation of functions to explicitly state metrics instead of `fairness_check_metrics()`. [(#29)](https://github.com/ModelOriented/fairmodels/issues/29)
* Fixed typos ([#27](https://github.com/ModelOriented/fairmodels/issues/27) and [#28](https://github.com/ModelOriented/fairmodels/issues/28))
* Changed conclusion drawn from density plot in `Basic Tutorial` (#26)

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
