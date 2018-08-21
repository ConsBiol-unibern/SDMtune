#' Title
#'
#' @param model
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
render_template <- function(model, folder) {
  template = system.file("templates", "modelOutput.Rmd", package = "SDMsel")

  dir.create(folder)
  folder <- paste0(getwd(), "/", folder)
  species <- gsub(" ", "_", tolower(model@presence@species))

  args <- c(paste0("--metadata=title:\"", model@presence@species, "\""))

  rmarkdown::render(template,
                    output_file = paste0(species, ".html"),
                    output_dir = folder,
                    params = list(model = model),
                    output_options = list(pandoc_args = args),
                    quiet = TRUE
                    )

  html <- paste0(folder, "/", species, ".html")
  browseURL(html)
}
