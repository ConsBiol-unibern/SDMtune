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
            cli::cli_h2("Object of class: {.cls {class(object)}}")

            cli::cli_par()
            cli::cli_text("Method: {.emph Boosted Regression Trees}")
            cli::cli_end()

            cli::cli_par()
            cli::cli_h3("Hyperparameters")
            cli::cli_end()

            cli::cli_par()
            cli::cli_li("{.field distribution}: {object@distribution}")
            cli::cli_li("{.field n.trees}: {object@n.trees}")
            cli::cli_li("{.field interaction.depth}: {object@interaction.depth}")
            cli::cli_li("{.field shrinkage}: {object@shrinkage}")
            cli::cli_li("{.field bag.fraction}: {object@bag.fraction}")
            cli::cli_end()
          })
