
# AdvancedRHT2025Lab05

<!-- badges: start -->
[![R-CMD-check](https://github.com/Cl4ryty/AdvancedRHT2025Lab05/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Cl4ryty/AdvancedRHT2025Lab05/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of AdvancedRHT2025Lab05 is to provide an R wrapper to the [thenmaps API](https://www.thenmap.net/) to allow for easy access to its data for further processing and analysis in R. 

## Installation

You can install the development version of AdvancedRHT2025Lab05 like so:

``` r
devtools::install_github("https://github.com/Cl4ryty/AdvancedRHT2025Lab05", build_vignettes=TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem - retrieving geo data from the API:

``` r
library(AdvancedRHT2025Lab05)
api <- thenmapsAPI(version="v2")
get_geo(api=api, dataset_name="se-7", date="2015")
```

