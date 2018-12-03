#' Model Report
#'
#' Make a model report that shows main results
#'
#' @param model SDMmodel object
#' @param type character. Output type, see \link{predict,Maxent-method} for
#' Maxent models or \link{predict.maxnet} for Maxnet models.
#' @param folder character. The name of the folder in which to save the output.
#' The folder is created in the working directory.
#' @param test SWD object with the test locations, default is NULL.
#' @param response_curves logical, if TRUE it plots the response curves in the
#' html output, default is FALSE.
#' @param jk logical, if TRUE it runs the jackknife test, default FALSE.
#' @param env \link{stack} or \link{brick}. If provided it computes and adds a
#' prediction map to the output, default is NULL.
#' @param clamp logical for clumping during prediction, used for response curves
#' and when **env** is provided, default is TRUE.
#'
#' @details The function produces a report similar to the one created by Maxent
#' software.
#'
#' @export
#' @importFrom utils menu
#' @importFrom crayon red green
#' @importFrom cli symbol
#'
#' @examples \dontrun{
#' modelReport(model, "cloglog", folder = "my_folder", response_curves = T)}
#'
#' @author Sergio Vignali
modelReport <- function(model, type, folder, test = NULL,
                        response_curves = FALSE, jk = FALSE, env = NULL,
                        clamp = TRUE) {

  if (file.exists(paste0(getwd(), "/", folder)))
    continue <- utils::menu(choices = c("Yes", "No"),
                            title = message(crayon::red(cli::symbol$fancy_question_mark),
                                            " The folder '", folder,
                                            "' already exists, do you want to overwrite it?"))
  if (continue == 1) {
    template = system.file("templates", "modelReport.Rmd", package = "SDMsel")

    dir.create(paste0(folder, "/plots"), recursive = TRUE, showWarnings = FALSE)
    folder <- paste0(getwd(), "/", folder)
    species <- gsub(" ", "_", tolower(model@presence@species))
    title <- paste(class(model@model), "model for", model@presence@species)
    args <- c(paste0("--metadata=title:\"", title, "\""))

    rmarkdown::render(template,
                      output_file = paste0(species, ".html"),
                      output_dir = folder,
                      params = list(model = model, type = type, test = test,
                                    folder = folder, env = env, jk = jk,
                                    response_curves = response_curves,
                                    clamp = clamp),
                      output_options = list(pandoc_args = args),
                      quiet = TRUE
                      )

    html <- paste0(folder, "/", species, ".html")
    model@html <- html
    browseURL(html)

    gc()
  }

  return(invisible(model))
}

