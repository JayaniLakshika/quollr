
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quollr

<img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/JayaniLakshika/quollr/workflows/R-CMD-check/badge.svg)](https://github.com/JayaniLakshika/quollr/workflows/R-CMD-check)
[![test](https://github.com/JayaniLakshika/quollr/workflows/test-coverage/badge.svg)](https://github.com/JayaniLakshika/quollr/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/JayaniLakshika/quollr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JayaniLakshika/quollr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `quollr` package provide functions to construct a model in the 2D
space based on 2D embedding and then lifts to the high dimensional
space. The package also provides a tools to visalise model in 2D space,
and visualise the fitted model overlaid on data using the tour
technique. Furthermore, facilitates to generate summary of high
dimensional distributions.

<!--For a thorough background and discussion on this work, please read our paper [link to the paper].
&#10;## Installation
&#10;You can install the released version of `quollr` from [CRAN](https://CRAN.R-project.org) with:
&#10;``` r
# install.packages("quollr") 
```
-->

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
# remotes::install_github("JayaniLakshika/quollr")
```

## Usage

Our approach involves dividing the high-dimensional data set into two
parts: a training set to construct the model and a test set to generate
model summaries. To implement our approach, first we use a 2D embedding
data set as the initial point. The output of our algorithm is a tour
that displays the model overlaid on high-dimensional data. Our algorithm
comprises two main phases:(1) generate the model in the 2D space, and
(2) generate the model in the high-dimensional space. This methodology
is available in this package.

<img src="man/figures/quollr-methodology.png" width="80%" style="display: block; margin: auto;" />

## About the name

**qu**estioning how a high-dimensional **o**bject **l**ooks in
**l**ow-dimensions using **r**

## Roadmap

- To learn more about the data preprocessing: [1. Data
  preprocessing](https://quollr.netlify.app/articles/algomain)
- To learn more about the main steps of the algorithm: [2. Algorithm for
  visualising the model overlaid with high-dimensional
  data](https://quollr.netlify.app/articles/algomain)
- To create the full hexagonal grid: [3. Algorithm for binning
  data](https://quollr.netlify.app/articles/visfullgrid)
- To generate model summaries: [4. Generating model
  summaries](https://quollr.netlify.app/articles/prediction)

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/quollr/LICENSE.md).
