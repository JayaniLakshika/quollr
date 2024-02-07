
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quollr

<img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/JayaniLakshika/quollr/workflows/R-CMD-check/badge.svg)](https://github.com/JayaniLakshika/quollr/workflows/R-CMD-check)
[![test](https://github.com/JayaniLakshika/quollr/workflows/test-coverage/badge.svg)](https://github.com/JayaniLakshika/quollr/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The `quollr` package provide functions to construct a model in the 2D
space based on 2D embedding and then lifts to the high dimensional
space. The package also provides a visualization that integrates this
model with high dimensional data using the tour technique. For a
thorough background and discussion on this work, please read our paper
\[link to the paper\].

## Installation

You can install the released version of `quollr` from
[CRAN](https://CRAN.R-project.org) with:

``` r
# install.packages("quollr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
# remotes::install_github("JayaniLakshika/quollr")
```

## Usage

<img src="man/figures/quollr-methodology.png" width="80%" style="display: block; margin: auto;" />

## About the name

**qu**estioning how a high-dimensional **o**bject **l**ooks in
**l**ow-dimensions using **r**

## Roadmap

- To learn more about the basic steps of the algorithm: [1. Main
  steps](https://quollr.netlify.app/articles/algomain)
- To create the full hexagonal grid: [2. Visualise full hexagonal
  grid](https://quollr.netlify.app/articles/visfullgrid)
- To generate 2D embeddings for test data: [3. Predict 2D
  embeddings](https://quollr.netlify.app/articles/prediction)

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/quollr/LICENSE.md).
