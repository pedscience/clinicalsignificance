
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clinicalsignificance

<!-- badges: start -->
<!-- badges: end -->

The goal of clinicalsignificance is to conduct analyses of clinical
significance in clinical intervention studies. In contrast to
*statistical* significance, which assesses if it is probable that there
is a treatment effect, *clinical* significance can be used to determine
if a treatment effect is of practical use or meaninful for patients.

## Installation

You can install the development version of clinicalsignificance from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pedscience/clinicalsignificance")
```

## Rationale

## Example

Given a tidy dataset, the employed instrument’s reliability and
descriptives (*M* and *SD*) of the functional population, the clinical
significance in a study can be easily assessed.

``` r
library(clinicalsignificance)

results <- claus_2020 %>% 
  clinical_significance(
    id = id, 
    time = time, 
    outcome = bdi, 
    pre = 1, 
    post = 4, 
    reliability = 0.81, 
    m_functional = 8, 
    sd_functional = 8, 
    type = "c"
  )

results
#> Clinical Significance Results (JT)
#> 
#> Category     |  n | Percent
#> ---------------------------
#> Recovered    | 10 |   0.250
#> Improved     |  9 |   0.225
#> Unchanged    | 21 |   0.525
#> Deteriorated |  0 |   0.000
#> Harmed       |  0 |   0.000
```

You can receive a detailed summary of the analysis by

``` r
summary(results)
#> 
#> Clinical Significance Results
#> 
#> There were 43 participants in the whole dataset of which 40 (93%) could be included in the analysis.
#> 
#> The JT method for calculating cutoffs and reliable change was chosen and the outcome variable was "bdi".
#> 
#> The cutoff type was "c" with a value of 21.6 based on the following population characteristics:
#> (with lower values representing a beneficial outcome)
#> 
#> Population Characteristics
#> 
#> M Clinical | SD Clinical | M Functional | SD Functional
#> -------------------------------------------------------
#> 35.48      | 8.16        | 8            | 8            
#> 
#> 
#> The instrument's reliability was set to 0.81 
#> 
#> Individual Level Results
#> 
#> Category     |  n | Percent
#> ---------------------------
#> Recovered    | 10 |   0.250
#> Improved     |  9 |   0.225
#> Unchanged    | 21 |   0.525
#> Deteriorated |  0 |   0.000
#> Harmed       |  0 |   0.000
```

or plot the results with

``` r
plot(results)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
