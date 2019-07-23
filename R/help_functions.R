#' Convert Old SWD object
#'
#' This is an help function to convert SWD objects created with an old version
#' of \pkg{SDMtune} (v <= 0.2.0) into the new format.
#'
#' @param object1 Old SWD object with presence or test locations.
#' @param object2 Old SWD object with absence/background locations.
#' @param fold logical, used internally, don't use it directly.
#'
#' @return An \code{\linkS4class{SWD}} object in the new format.
#' @export
#'
#' @author Sergio Vignali
old2NewSWD <- function(object1, object2 = NULL, fold = NULL) {

  if (!is.null(object2)) {
    if (ncol(object1@data) != ncol(object2@data))
      stop("object1 and object2 have data with different number of columns.")
    if (!all(colnames(object1@data) == colnames(object2@data)))
      stop("object1 and object2 have data with different colnames.")
  }

  species = object1@species
  data <- object1@data
  coords <- object1@coords
  if (!is.null(fold))
    coords <- coords[fold, ]
  pa <- rep(1, nrow(coords))

  if (!is.null(object2)) {
    data <- rbind(data, object2@data)
    rownames(data) <- NULL
    coords <- rbind(coords, object2@coords)
    rownames(coords) <- NULL
    pa <- c(pa, rep(0, nrow(object2@coords)))
  }

  swd <- SWD(species = species,
             data = data,
             coords = coords,
             pa = pa)

  return(swd)
}

#' Convert Old SDMmodel object
#'
#' This is an help function to convert SDMmodel objects created with an old
#' version of \pkg{SDMtune} (v <= 0.2.0) into the new format.
#'
#' @param model old SDMmodel object.
#' @param fold logical, used internally, don't use it directly.
#'
#' @return An \code{\linkS4class{SDMmodel}} object in the new format.
#' @export
#'
#' @author Sergio Vignali
old2NewSDMmodel <- function(model, fold = NULL) {
  data <- old2NewSWD(model@p, model@a, fold)
  output <- SDMmodel(data = data, model = model@model)

  return(output)
}

#' Convert Old SDMmodelCV object
#'
#' This is an help function to convert SDMmodelCV objects created with an old
#' version of \pkg{SDMtune} (v <= 0.2.0) into the new format.
#'
#' @param model old SDMmodelCV object.
#'
#' @return An \code{\linkS4class{SDMmodelCV}} object in the new format.
#' @export
#'
#' @author Sergio Vignali
old2NewSDMmodelCV <- function(model) {
  data <- SWD(species = model@p@species,
              data = model@p@data,
              coords = model@p@coords,
              pa = rep(1, nrow(model@p@data)))
  data@data <- rbind(data@data, model@a@data)
  rownames(data@data) <- NULL
  data@coords <- rbind(data@coords, model@a@coords)
  rownames(data@coords) <- NULL
  data@pa <- c(data@pa, rep(0, nrow(model@a@coords)))

  k <- length(model@models)
  models <- vector("list", length = k)
  folds <- matrix(TRUE, nrow = nrow(data@coords), ncol = k)

  for (i in 1:k) {
    fold <- model@folds != i
    models[[i]] <- old2NewSDMmodel(model@models[[i]], fold = fold)
    folds[, i] <- c(fold, rep(TRUE, nrow(model@a@data)))
  }

  output <- SDMmodelCV(data = data, models = models, folds = folds)

  return(output)
}

#' Convert Old SDMtune object
#'
#' This is an help function to convert SDMtune objects created with an old
#' version of \pkg{SDMtune} (v <= 0.2.0) into the new format.
#'
#' @param object old SDMtune object.
#'
#' @return An \code{\linkS4class{SDMtune}} object in the new format.
#' @export
#'
#' @author Sergio Vignali
old2NewSDMtune <- function(object) {
  l <- length(object@model)
  models <- vector("list", length = l)

  if (.hasSlot(object@models[[1]], "fold")) {
    f <- "old2NewSDMmodelCV"
  } else {
    f <- "old2NewSDMmodel"
  }

  for (i in 1:l) {
    models[[i]] <- do.call(f, args = object@models[[i]])
  }

  output <- SDMtune(results = object@results, models = models)

  return(output)
}
