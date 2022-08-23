setOldClass("randomForest")
#' Random Forest
#'
#' This Class represents a Random Forest model objects and hosts all the
#' information related to the model.
#'
#' @slot mtry integer. Number of variable randomly sampled.
#' @slot ntree integer. Number of grown trees.
#' @slot nodesize integer. Minimum size of terminal nodes.
#' @slot model \link{randomForest}. The randomForest model object.
#'
#' @details See \link{randomForest} for the meaning of the slots.
#'
#' @rdname RF-class
#' @export
#'
#' @author Sergio Vignali
RF <- setClass("RF",
               slots = c(
                 mtry = "numeric",
                 ntree = "numeric",
                 nodesize = "numeric",
                 model = "randomForest")
)

#' @param object RF object
#' @rdname RF-class
setMethod("show",
          signature = "RF",
          definition = function(object) {
            cat("Class   :", class(object), "\n")
            cat("mtry    :", object@mtry, "\n")
            cat("ntree   :", object@ntree, "\n")
            cat("nodesize:", object@nodesize, "\n")
          })
