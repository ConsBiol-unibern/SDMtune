setClassUnion("model", c("Maxent", "Maxnet", "ANN", "RF", "BRT"))
#' SDMmodel
#'
#' This Class represents an SDMmodel object and hosts all the information
#' related to the model.
#'
#' @slot data \linkS4class{SWD} object. The data used to train the model.
#' @slot model An object of class \linkS4class{ANN}, \linkS4class{BRT},
#' \linkS4class{RF}, \linkS4class{Maxent} or \linkS4class{Maxnet}.
#'
#' @rdname SDMmodel-class
#' @include SWD-class.R Maxent-class.R Maxnet-class.R RF-class.R BRT-class.R
#' ANN-class.R
#' @export
#'
#' @author Sergio Vignali
SDMmodel <- setClass("SDMmodel",
  representation(
    data = "SWD",
    model = "model")
)

#' @param object SDMmodel object
#' @rdname SDMmodel-class
setMethod(
  "show",
  signature = "SDMmodel",
  definition = function(object) {

    tunable_hypers <- getTunableArgs(object)
    cont_vars <- names(Filter(is.numeric, object@data@data))

    if (length(cont_vars) == 0)
      cont_vars <- NA

    cat_vars <- names(Filter(is.factor, object@data@data))

    if (length(cat_vars) == 0)
      cat_vars <- NA

    n_p <- sum(object@data@pa == 1)
    n_a <- sum(object@data@pa == 0)

    cli::cli_h2("Object of class: {.cls {class(object)}}")

    cli::cli_par()
    cli::cli_text("Method: {.emph { .get_method(object)}}")
    cli::cli_end()

    cli::cli_h3("Hyperparameters")

    for (i in seq_along(tunable_hypers)) {
      h <- slot(object@model, tunable_hypers[i])
      cli::cli_li("{.field {tunable_hypers[i]}}: {.val {h}}")
    }

    cli::cli_h3("Info")

    cli::cli_li("{.field Species}: {.emph {object@data@species}}")
    cli::cli_li("{.field Presence locations}: {.val {n_p}}")
    cli::cli_li("{.field Absence locations}: {.val {n_a}}")

    cli::cli_h3("Variables")

    cli::cli_li("{.field Continuous}: {.val {cont_vars}}")
    cli::cli_li("{.field Categorical}: {.val {cat_vars}}")
  }
)
