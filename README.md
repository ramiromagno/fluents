
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fluents <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/fluents)](https://CRAN.R-project.org/package=fluents)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `{fluents}` is to be an opinionated R package
re-implementation of [grind.R](https://tbb.bio.uu.nl/rdb/grindR.html) by
Rob de Boer, Theoretical Biology and Bioinformatics, University of
Utrecht. `grind.R` is a port from the same author C-based program [GRIND
(GReat INtegrator Differential
equations)](https://tbb.bio.uu.nl/rdb/grindC.html) to R.

`{fluents}` aims at packing the same functionality as `grind.R`,
departing from `grind.R` in a few ways:

- `{fluents}` is an R package, `grind.R` is a script.
- `{fluents}` is under public version control at GitHub, facilitating
  open collaboration and easy distribution.
- `{fluents}`’s API has more functions that do less each, while
  `grind.R`’s has fewer functions that do more each.

## Installation

You can install the current version of `{fluents}` with:

``` r
# install.packages("remotes")
remotes::install_github("ramiromagno/fluents")
```

## Example

TODO

``` r
library(fluents)
#> 
#> Attaching package: 'fluents'
#> The following object is masked from 'package:base':
#> 
#>     solve
## TODO
```

## From `grind.R` to `{fluents}`

| grind.R | fluents   |
|---------|-----------|
| `run()` | `solve()` |
