#' SDMtune: A package for tuning Species Distribution Models.
#'
#' **SDMtune** provides a user-friendly framework that enables the training and
#' the evaluation of species distribution models (SDMs). The package implements
#' functions for data driven variable selection and model tuning and includes
#' numerous utilities to display the results. All the functions used to select
#' variables or to tune model hyperparameters have an interactive real-time
#' chart displayed in the RStudio viewer pane during their execution. SDMtune
#' uses its own script to predict MaxEnt models, resulting in much faster
#' predictions for large datasets compared to native predictions from the use of
#' the Java software. This reduces considerably the computation time when tuning
#' the model using the AICc. At the moment only the Maximum Entropy method is
#' available using the Java implementation through the \link{dismo} package and
#' the R implementation through the \link{maxnet} package.
#' Visit the [package website](https://consbiol-unibern.github.io/SDMtune/)
#' and learn how to use **SDMtune** starting from the first article
#' [Prepare data for the analysis](https://consbiol-unibern.github.io/SDMtune/articles/articles/prepare-data.html).
#'
#' @docType package
#' @name SDMtune-pkg
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @useDynLib SDMtune
NULL
