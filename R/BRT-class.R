setOldClass("gbm")
#' Boosted Regression Tree
#'
#' This Class represents a Boosted Regression Tree model objects and hosts all
#' the information related to the model.
#'
#' @slot distribution character. Name of the used distribution.
#' @slot ntree integer. Maximum number of grown trees.
#' @slot interaction.depth integer. Maximum depth of each tree.
#' @slot lr numeric. The shrinkage parameter.
#' @slot bag.fraction numeric. Random fraction of data used in the tree
#' expansion.
#' @slot model \code{\link{gbm}}. The Boosted Regression Tree model object.
#'
#' @details See \code{\link{gbm}} for the meaning of the slots.
#'
#' @exportClass BRT
#'
#' @author Sergio Vignali
BRT <- setClass("BRT",
                slots = c(
                  distribution = "character",
                  ntree = "numeric",
                  interaction.depth = "numeric",
                  lr = "numeric",
                  bag.fraction = "numeric",
                  model = "gbm")
)

setMethod("show",
          signature = "BRT",
          definition = function(object) {
            cat("Class            :", class(object), "\n")
            cat("distribution     :", object@distribution, "\n")
            cat("ntree            :", object@ntree, "\n")
            cat("interaction.depth:", object@interaction.depth, "\n")
            cat("lr               :", object@lr, "\n")
            cat("bag.fraction     :", object@bag.fraction, "\n")
          })
