#' Title
#'
#' @param model
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
modelReport <- function(model, type, test, folder) {
  template = system.file("templates", "modelReport.Rmd", package = "SDMsel")

  dir.create(paste0(folder, "/plots"), recursive = TRUE)
  folder <- paste0(getwd(), "/", folder)
  plot_folder <- paste0(folder, "/plots")
  species <- gsub(" ", "_", tolower(model@presence@species))
  title <- paste(class(model@model), "model for", model@presence@species)
  args <- c(paste0("--metadata=title:\"", title, "\""))

  rmarkdown::render(template,
                    output_file = paste0(species, ".html"),
                    output_dir = folder,
                    params = list(model = model, type = type, test = test,
                                  plot_folder = plot_folder),
                    output_options = list(pandoc_args = args),
                    quiet = TRUE
                    )

  # # ggsave in a Rmd file doesn't save the device...
  # for (file in list.files(path = plot_folder))
  #   file.rename(file, paste0(file, ".png"))

  html <- paste0(folder, "/", species, ".html")
  browseURL(html)

  return(invisible(html))
}

