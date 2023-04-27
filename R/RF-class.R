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
            cli::cli_h2("Object of class: {.cls {class(object)}}")

            cli::cli_par()
            cli::cli_text("Method: {.emph Random Forest}")
            cli::cli_end()

            cli::cli_h3("Hyperparameters")

            cli::cli_li("{.field mtry}: {.val {object@mtry}}")
            cli::cli_li("{.field ntree}: {.val {object@ntree}}")
            cli::cli_li("{.field nodesize}: {.val {object@nodesize}}")
          })
