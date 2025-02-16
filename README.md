
# Tyler’s Analysis Tools <a href="https://github.com/BCC-Biostats/tat"><img src="man/figures/tathex.png" align="right" height="138" /></a>

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![GitHub issues](https://img.shields.io/github/issues/BCC-Biostats/tat)
![GitHub closed
issues](https://img.shields.io/github/issues-closed/BCC-Biostats/tat)
![GitHub
contributors](https://img.shields.io/github/contributors/BCC-Biostats/tat)
![GitHub last
commit](https://img.shields.io/github/last-commit/BCC-Biostats/tat)
![GitHub repo
size](https://img.shields.io/github/repo-size/BCC-Biostats/tat)

## Overview

tat is a package for holding my biostatistics analysis tools. Currently
has 2 parts:

- Claims Data
- Survival Analysis

## Installation

To install the package, run the following commands:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("BCC-Biostats/tat")   
```

## Claims Data

### *cd_find_ids()*: Find codes in a claims data set across several columns

This function will help you find if a code is present in a claims data
set across several columns. It will return a tibble with a list column
of the codes found and a logical column indicating if any of the codes
were found.

Example:

``` r
#' dat <- tibble(
#'   id = 1:3,
#'   code1 = c("A", "B", "C"),
#'   code2 = c("B", "C", "D")
#' )
#' cd_find_ids(dat, c("code1", "code2"), c("A", "B"))
#' # A tibble: 3 x 2
#' # codes_found any_codes
#' # <list>      <lgl>
#' # 1 <chr [1]>   TRUE
#' # 2 <chr [1]>   TRUE
#' # 3 <chr [0]>   FALSE
```

## Survival Analysis
