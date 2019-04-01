#' SDMtune: A package for tuning Species Distribution Models.
#'
#' **SDMtune** provides a framework that facilitates users in preparing data for
#' analysis, train and evaluate models. It also implements functions for data
#' driven variable selection and model tuning and includes some utilities to
#' display results (at the moment it implements MaxEnt and Maxnet models). All
#' the functions used to select variables or to tune the model hyperparameters
#' have a real-time chart that is displayed in the RStudio viewer pane during
#' the function execution. SDMtune uses its own script to predict MaxEnt models
#' that results to be much faster for large datasets than native prediction made
#' using Java software. This reduce considerably the computation time when
#' tuninig the model using the AICc.
#'
#' @docType package
#' @name SDMtune-pkg
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @useDynLib SDMtune
NULL
