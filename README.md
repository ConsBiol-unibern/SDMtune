
<!-- README.md is generated from README.Rmd. Please edit that file -->
SDMtune <img src="logo.svg" align="right" alt="" width="120" />
===============================================================

**SDMtune** provides a framework that facilitates users in preparing data for analysis, train and evaluate models. It also implements functions for data driven variable selection and model tuning and includes some utilities to display results (at the moment only the Maximum Entropy method is available using the Java implementation through the "dismo" package and the R implementation through the "maxnet" package). All the functions used to select variables or to tune model hyperparameters have an interactive real-time chart that is displayed in the RStudio viewer pane during the function execution. SDMtune uses its own script to predict MaxEnt models that results to be much faster for large datasets than native prediction made using the Java software. This reduces considerably the computation time when tuninig the model using the AICc.

Installation
------------

You can get the latest development version from github:

``` r
devtools::install_github("sgvignali/SDMtune")
```

Real-time charts
----------------

Real time charts displaying the training and validation metrics are displayed in the RStudio viewer pane during the execution of the tuning and variable selection functions.

<img src="realtime-chart.gif" alt="" />
