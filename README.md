
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quollr

<img src="man/figures/logo.png" align="right" height="139" alt="" />

Nonlinear dimension reduction (NLDR) techniques provide a
low-dimensional representation of high-dimensional data by applying a
non-linear transformation. The complexity of the transformations and
data structure can create wildly different representations depending on
the method and parameter choices. It is difficult to determine whether
any are accurate, which is best, or whether they have missed structure.
To help assess the NLDR and decide on which, if any, is best, we have
developed an algorithm to create a model that is then used to display as
a wireframe in high dimensions. The `quollr` package provides functions
to implement the algorithm.

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
install.packages("remotes")
remotes::install_github("JayaniLakshika/quollr")
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
  preprocessing](https://jayanilakshika.github.io/quollr/articles/quollr1dataprocessing.html)
- To learn more about the main steps of the algorithm: [2. Algorithm for
  visualising the model overlaid on high-dimensional
  data](https://jayanilakshika.github.io/quollr/articles/quollr2algo.html)
- To create the full hexagonal grid: [3. Algorithm for binning
  data](https://jayanilakshika.github.io/quollr/articles/quollr3hexbin.html)
- To generate model summaries: [4. Generating model
  summaries](https://jayanilakshika.github.io/quollr/articles/quollr4summary.html)

## Copyright

This package is licensed under the [MIT
license](https://github.com/JayaniLakshika/quollr/tree/main?tab=MIT-2-ov-file).
