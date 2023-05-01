#' SDMmodelCV
#'
#' This Class represents an SDMmodel model object with replicates and hosts all
#' the models trained during the cross validation.
#'
#' @slot models list. A list containing all the models trained during the cross
#' validation.
#' @slot data \linkS4class{SWD} object. Full dataset used to make the
#' partitions.
#' @slot folds list with two matrices, the first for the training and the second
#' for the testing dataset. Each column of one matrix represents a fold with
#' `TRUE` for the locations included in and `FALSE` excluded from the partition.
#'
#' @rdname SDMmodelCV-class
#' @include SWD-class.R
#' @export
#'
#' @author Sergio Vignali
SDMmodelCV <- setClass("SDMmodelCV",
                       representation(
                         models = "list",
                         data = "SWD",
                         folds = "list"
                       )
)

#' @param object SDMmodelCV object
#' @rdname SDMmodelCV-class
setMethod(
  "show",
  signature = "SDMmodelCV",
  definition = function(object) {

    tunable_hypers <- getTunableArgs(object@models[[1]])
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
      h <- slot(object@models[[i]]@model, tunable_hypers[i])
      cli::cli_li("{.field {tunable_hypers[i]}}: {.val {h}}")
    }

    cli::cli_h3("Info")

    cli::cli_li("{.field Species}: {.emph {object@data@species}}")
    cli::cli_li("{.field Replicates}: {.val {length(object@models)}}")
    cli::cli_li("{.field Total presence locations}: {.val {n_p}}")
    cli::cli_li("{.field Total absence locations}: {.val {n_a}}")

    cli::cli_h3("Variables")

    cli::cli_li("{.field Continuous}: {.val {cont_vars}}")
    cli::cli_li("{.field Categorical}: {.val {cat_vars}}")
  }
)
