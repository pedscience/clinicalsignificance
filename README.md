
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clinicalsignificance <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/clinicalsignificance)](https://cran.r-project.org/package=clinicalsignificance)
[![](http://cranlogs.r-pkg.org/badges/grand-total/clinicalsignificance)](https://cran.r-project.org/package=clinicalsignificance)

[![R-CMD-check](https://github.com/pedscience/clinicalsignificance/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pedscience/clinicalsignificance/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of this package is to provide all necessary tools for analyses
of clinical significance in clinical intervention studies. In contrast
to *statistical* significance, which assesses if it is probable that
there is a treatment effect, *clinical* significance can be used to
determine if a treatment effect is of practical use or meaningful for
patients.

## Installation

You can install clinicalsignificance from CRAN and the development
version from GitHub:

``` r
install.packages("clinicalsignificance")

# Or you install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("pedscience/clinicalsignificance")
```

## Example

Given a tidy dataset, the employed instrument’s reliability and a
minimal important difference for that instrument, a clinical
significance analysis can be calculated with the following function
call.

``` r
library(clinicalsignificance)

results <- claus_2020 |>
  cs_anchor(
    id = id,
    time = time,
    outcome = bdi,
    pre = 1,
    post = 4,
    mid_improvement = 7
  )

results
#> 
#> ── Clinical Significance Results ──
#> 
#> Individual anchor-based approach with a 7 point decrease in instrument scores
#> indicating a clinical significant improvement.
#> Category     |  n | Percent
#> ---------------------------
#> Improved     | 25 |    0.62
#> Unchanged    | 11 |    0.28
#> Deteriorated |  4 |    0.10
```

You can receive a detailed summary of the analysis by

``` r
summary(results)
#> 
#> ── Clinical Significance Results ──
#> 
#> Individual anchor-based analysis of clinical significance with a 7 point
#> decrease in instrument scores (bdi) indicating a clinical significant
#> improvement.
#> There were 43 participants in the whole dataset of which 40 (93%) could be
#> included in the analysis.
#> 
#> ── Individual Level Results
#> Category     |  n | Percent
#> ---------------------------
#> Improved     | 25 |    0.62
#> Unchanged    | 11 |    0.28
#> Deteriorated |  4 |    0.10
```

or plot the results with

``` r
plot(results)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="80%" style="display: block; margin: auto;" />
