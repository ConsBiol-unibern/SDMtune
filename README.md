
<!-- README.md is generated from README.Rmd. Please edit that file -->
SDMtune <img src="man/figures/logo.svg" align="right" alt="" width="120" />
===========================================================================

[![Travis-CI build status](https://travis-ci.org/ConsBiol-unibern/SDMtune.svg?branch=master)](https://travis-ci.org/ConsBiol-unibern/SDMtune) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/sgvignali/SDMtune?branch=master&svg=true)](https://ci.appveyor.com/project/sgvignali/SDMtune) [![Coverage status](https://codecov.io/gh/sgvignali/SDMtune/branch/master/graph/badge.svg)](https://codecov.io/github/sgvignali/SDMtune?branch=master) [![CRAN Status](https://www.r-pkg.org/badges/version/SDMtune)](https://cran.r-project.org/package=SDMtune) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/SDMtune)](http://www.r-pkg.org/pkg/SDMtune) [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v1.4%20adopted-ff69b4.svg)](code-of-conduct.md)

**SDMtune** provides a user-friendly framework that enables the training and the evaluation of species distribution models (SDMs). The package implements functions for data driven variable selection and model tuning and includes numerous utilities to display the results. All the functions used to select variables or to tune model hyperparameters have an interactive real-time chart displayed in the RStudio viewer pane during their execution. SDMtune uses its own script to predict MaxEnt models, resulting in much faster predictions for large datasets compared to native predictions from the use of the Java software. This reduces considerably the computation time when tuning the model using the AICc. At the moment only the Maximum Entropy method is available using the Java implementation through the “dismo” package and the R implementation through the “maxnet” package.
Visit the [package website](https://sgvignali.github.io/SDMtune/) and learn how to use **SDMtune** starting from the first article [Prepare data for the analysis](https://sgvignali.github.io/SDMtune/articles/articles/prepare_data.html).

Installation
------------

You can install the latest development version from GitHub:

``` r
devtools::install_github("sgvignali/SDMtune")
```

Real-time charts
----------------

Real-time charts displaying the training and the validation metrics are displayed in the RStudio viewer pane during the execution of the tuning and variable selection functions.

<img src="man/figures/realtime-chart.gif" alt="" />

Code of conduct
---------------

Please note that this project follows a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).
