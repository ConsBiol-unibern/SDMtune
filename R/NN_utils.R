#' Save NN
#'
#' Save a SDMmodel object containing a NN model.
#'
#' @param model SDMmodel object.
#' @param file_name character. The name of the file in which to save the model,
#' default is NULL and it uses the name of the species.
#'
#' @details The function creates two files, a hdf5 file for the serializes keras
#' object and a Rds file for the SDMmodel object. The file are saved in the
#' working directory.
#'
#' @export
#' @importFrom keras save_model_hdf5
#'
#' @examples \dontrun{saveNN(model, "myNN")}
#'
#' @author Sergio Vignali
saveNN <- function(model, file_name = NULL) {

  if (class(model@model) != "NN")
    stop("Model doesn't contain a NN object!")

  if (is.null(file_name))
    file_name <- gsub(" ", "_", model@presence@species)

  keras::save_model_hdf5(model@model@model,
                         filepath = paste0(getwd(), "/", file_name))
  saveRDS(model, file = paste0(file_name, ".Rds"))
}

#' Load NN
#'
#' Load a NN model in R from the files saved in the working directory.
#'
#' @param file_name The name of the files without the extension in which is
#' saved the object. The function expects to have the file in the working
#' directory.
#'
#' @return The created SDMmodel object containing the NN model.
#' @export
#' @importFrom keras load_model_hdf5
#'
#' @examples \dontrun{x <- loadNN("my_model")}
#'
#' @author Sergio Vignali
loadNN <- function(file_name) {
  model <- readRDS(paste0(file_name, ".Rds"))
  model@model@model <- keras::load_model_hdf5(paste0(getwd(), "/", file_name))
  return(model)
}

#' @importFrom keras model_to_yaml model_from_yaml
reshape_input <- function(model, input_units) {

  old_units <- get_input_units(model)
  yaml <- keras::model_to_yaml(model@model@model)
  yaml <- sub(paste("batch_input_shape: !!python/tuple \\[null,", old_units),
              paste("batch_input_shape: !!python/tuple \\[null,", input_units),
              yaml)
  new_model <- keras::model_from_yaml(yaml)
  return(new_model)
}

get_input_units <- function(model) {
  cont_vars <- names(Filter(is.numeric, model@presence@data))
  cat_vars <- names(Filter(is.factor, model@presence@data))
  units <- length(cont_vars)
  for (j in 1:length(cat_vars)) {
    units <- units + length(unlist(model@model@levels[cat_vars[j]]))
  }
  return(units)
}

#' @importFrom keras %>% keras_model_sequential layer_dense regularizer_l2
parseNNConf <- function(conf, reg, input_units) {
  model <- keras_model_sequential()
  for (i in 1:length(conf)) {
    if (i == 1) {
      model %>% layer_dense(units = conf[[i]][1],
                            activation = conf[[i]][2],
                            regularizer_l2(reg),
                            input_shape = input_units)
    } else {
      model %>% layer_dense(units = conf[[i]][1],
                            activation = conf[[i]][2],
                            regularizer_l2(reg))
    }
  }
  model %>% layer_dense(units = 1, activation = "sigmoid", regularizer_l2(reg))
  return(model)
}

format_data <- function(x, means, stds, levels) {

  cont_vars <- names(Filter(is.numeric, x))
  cat_vars <- names(Filter(is.factor, x))

  x[cont_vars] <- scale(x[cont_vars], center = means, scale = stds)

  if (length(cat_vars) > 0) {
    for (cat in cat_vars) {
      one_hot <- one_hot(x[, cat], unlist(levels[cat]))
      colnames(one_hot) <- paste0(cat, "_", 1:ncol(one_hot))
      x[cat] <- NULL
      x <- cbind(x, one_hot)
    }
  }

  return(x)
}
