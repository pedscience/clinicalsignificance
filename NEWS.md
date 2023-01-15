# clinicalsignificance (development version)
## New features
* You can now specify a significance level at which you want the analysis to be based on with the `significance_level` argument. The default is `significance_level = 0.05`. Note that for this is the Phi max level when you set `method = "HA"` as outlined in their article.

## Breaking changes
* When `method = "HLM`, participants with three or more data points will be used. Before, participants with at least three data points (i.e., 4 or more) were used.

## Minor improvements and fixes
* Change the whole package to use R's native pipe operator `|>`
* Remove unnecessary dependency on `magrittr`

# clinicalsignificance 1.2.1
## Breaking changes
* Remove default theming from `check_cutoff()` as in other plot functions

## Minor improvements and fixes
* Improve documentation of datasets
* Include examples for `summary()` and `plot()` method

# clinicalsignificance 1.2.0
## Breaking changes
* Remove default theming from `plot()` to give the user more control over the overall appearance

## Minor improvements and fixes
* Further implementation of tidyeval principles since `aes_()` was deprecated
* Small improvements in vignettes
* Better documentation for some functions


# clinicalsignificance 1.1.0
* Fix HLM method empirical bayes estimation (#2). This now relies on the lme4 package
* Consider the instrument direction for group level summaries in the HA method (#1) which was not done in the initial release


# clinicalsignificance 1.0.1
* Added a `NEWS.md` file to track changes to the package.
* Added package URLs to DESCRIPTION, including one for bug reports
* New package logo
