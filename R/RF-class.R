setOldClass("randomForest")
#' Random Forest
#'
#' This Class represents a Random Forest model objects and hosts all the
#' information related to the model.
#'
#' @slot mtry numeric. Number of variable randomly sampled.
#' @slot ntree numeric. Number of grown trees.
#' @slot model \code{\link{randomForest}}. The randomForest model object.
#'
#' @details See \code{\link{randomForest}} for the meaning of the slots.
#'
#' @exportClass RF
#'
#' @author Sergio Vignali
RF <- setClass("RF",
               slots = c(
                 mtry = "numeric",
                 ntree = "numeric",
                 model = "randomForest")
)

setMethod("show",
          signature = "RF",
          definition = function(object) {
            cat("Class:", class(object), "\n")
            cat("mtry :", object@mtry, "\n")
            cat("ntree:", object@ntree, "\n")
          })
