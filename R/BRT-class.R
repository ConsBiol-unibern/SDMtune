setOldClass("gbm")
#' Boosted Regression Tree
#'
#' This Class represents a Boosted Regression Tree model objects and hosts all
#' the information related to the model.
#'
#' @slot distribution character. Name of the used distribution.
#' @slot n.trees integer. Maximum number of grown trees.
#' @slot interaction.depth integer. Maximum depth of each tree.
#' @slot shrinkage numeric. The shrinkage parameter.
#' @slot bag.fraction numeric. Random fraction of data used in the tree
#' expansion.
#' @slot model \link{gbm}. The Boosted Regression Tree model object.
#'
#' @details See \link{gbm} for the meaning of the slots.
#'
#' @rdname BRT-class
#' @export
#'
#' @author Sergio Vignali
BRT <- setClass("BRT",
                slots = c(
                  distribution = "character",
                  n.trees = "numeric",
                  interaction.depth = "numeric",
                  shrinkage = "numeric",
                  bag.fraction = "numeric",
                  model = "gbm")
)

#' @param object BRT object
#' @rdname BRT-class
setMethod("show",
          signature = "BRT",
          definition = function(object) {
            cat("Class            :", class(object), "\n")
            cat("distribution     :", object@distribution, "\n")
            cat("n.trees          :", object@n.trees, "\n")
            cat("interaction.depth:", object@interaction.depth, "\n")
            cat("shrinkage        :", object@shrinkage, "\n")
            cat("bag.fraction     :", object@bag.fraction, "\n")
          })
