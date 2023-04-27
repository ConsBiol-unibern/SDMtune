setOldClass("maxnet")
#' Maxnet
#'
#' This Class represents a Maxnet model objects and hosts all the information
#' related to the model.
#'
#' @slot reg numeric. The value of the regularization multiplier used to train
#' the model.
#' @slot fc character. The feature class combination used to train the model.
#' @slot model maxnet. The maxnet model object.
#'
#' @rdname Maxnet-class
#' @export
#'
#' @author Sergio Vignali
Maxnet <- setClass("Maxnet",
                   slots = c(
                     reg = "numeric",
                     fc = "character",
                     model = "maxnet")
)

#' @param object Maxnet object
#' @rdname Maxnet-class
setMethod("show",
          signature = "Maxnet",
          definition = function(object) {
            cli::cli_h2("Object of class: {.cls {class(object)}}")

            cli::cli_par()
            cli::cli_text("Method: {.emph Maxnet}")
            cli::cli_end()

            cli::cli_h3("Hyperparameters")

            cli::cli_li("{.field reg}: {.val {object@reg}}")
            cli::cli_li("{.field fc}: {.val {object@fc}}")
          })
