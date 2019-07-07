#' Model Report
#'
#' Make a report that shows the main results.
#'
#' @param model \linkS4class{SDMmodel} object.
#' @param type character. Output type, see \code{\link{predict,SDMmodel-method}}
#' for more details.
#' @param folder character. The name of the folder in which to save the output.
#' The folder is created in the working directory.
#' @param test \linkS4class{SWD} object with the test locations, default is
#' \code{NULL}.
#' @param response_curves logical, if \code{TRUE} it plots the response curves
#' in the html output, default is \code{FALSE}.
#' @param jk logical, if \code{TRUE} it runs the jackknife test, default
#' \code{FALSE}.
#' @param env \code{\link[raster]{stack}}. If provided it computes and adds a
#' prediction map to the output, default is \code{NULL}.
#' @param clamp logical for clumping during prediction, used for response curves
#' and for the prediction map, default is \code{TRUE}.
#' @param permut integer. Number of permutations, default is 10.
#'
#' @details The function produces a report similar to the one created by MaxEnt
#' software.
#'
#' @export
#' @importFrom utils menu
#' @importFrom crayon red green
#' @importFrom cli symbol rule
#' @importFrom utils browseURL
#' @importFrom htmltools HTML
#' @import kableExtra
#'
#' @author Sergio Vignali
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence locations
#' p_coords <- condor[, 1:2]
#'
#' # Prepare background locations
#' bg_coords <- dismo::randomPoints(predictors, 5000)
#'
#' # Create SWD object
#' presence <- prepareSWD(species = "Vultur gryphus", coords = p_coords,
#'                        env = predictors, categorical = "biome")
#' bg <- prepareSWD(species = "Vultur gryphus", coords = bg_coords,
#'                  env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(presence, test = 0.2)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxent", p = train, a = bg, fc = "l")
#'
#' # Create the report
#' modelReport(model, type = "cloglog", folder = "my_folder", test = test,
#'             response_curves = TRUE, jk = TRUE, env = predictors)
#' }
modelReport <- function(model, type, folder, test = NULL,
                        response_curves = FALSE, jk = FALSE, env = NULL,
                        clamp = TRUE, permut = 10) {

  if (file.exists(paste0(getwd(), "/", folder))) {
    msg <- message(crayon::red(cli::symbol$fancy_question_mark),
                   " The folder '", folder,
                   "' already exists, do you want to overwrite it?")
    continue <- utils::menu(choices = c("Yes", "No"),
                            title = msg)
  } else {
    continue <- 1
  }
  if (continue == 1) {
    template <- system.file("templates", "modelReport.Rmd", package = "SDMtune")

    folder <- file.path(getwd(), folder)
    dir.create(file.path(folder, "plots"), recursive = TRUE,
               showWarnings = FALSE)
    species <- gsub(" ", "_", tolower(model@p@species))
    title <- paste(class(model@model), "model for", model@p@species)
    args <- c(paste0("--metadata=title:\"", title, "\""))
    output_file <- paste0(species, ".html")

    rmarkdown::render(template,
                      output_file = output_file,
                      output_dir = folder,
                      params = list(model = model, type = type, test = test,
                                    folder = folder, env = env, jk = jk,
                                    response_curves = response_curves,
                                    clamp = clamp, permut = permut),
                      output_options = list(pandoc_args = args),
                      quiet = TRUE
                      )
  }
  utils::browseURL(file.path(folder, output_file))
}
