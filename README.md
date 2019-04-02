
<!-- README.md is generated from README.Rmd. Please edit that file -->
SDMtune <img src="logo.svg" align="right" alt="" width="120" />
===============================================================

**SDMtune** provides a framework that facilitates preparing data for analysis, train and evaluate SDMs (at the moment the Maximum Entropy method is available using the java implementation through the dismo package or the R implementation through the maxnet package). It also includes utilities to display results and functions for variable selection and model tuning. All the functions used to select variables or to tune the model hyperparameters have a real-time chart that is displayed in the RStudio viewer pane during the function execution. **SDMtune** uses its own script to predict MaxEnt models that performs much faster for large datasets than native predictions made using Java software. This reduces considerably the computation time when tuning the model, especially when optimizing the **AICc**.

Installation
------------

You can get the latest development version from github:

``` r
devtools::install_github("sgvignali/SDMtune")
```

Real time charts
----------------

Real time charts displaying the training and validation metrics are displayed in the RStudio viewer pane during the execution of the tuning and variable selection functions.

<img src="realtime-chart.gif" alt="" />
