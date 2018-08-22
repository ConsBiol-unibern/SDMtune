#' Title
#'
#' @param model
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
render_template <- function(model, test, folder) {
  template = system.file("templates", "modelOutput.Rmd", package = "SDMsel")

  dir.create(paste0(folder, "/plots"), recursive = TRUE)
  folder <- paste0(getwd(), "/", folder)
  plot_folder <- paste0(folder, "/plots")
  species <- gsub(" ", "_", tolower(model@presence@species))
  title <- paste(class(model@model), "model for", model@presence@species)
  args <- c(paste0("--metadata=title:\"", title, "\""))

  roc <- plotROC(model, test = test)
  ggsave(filename = "ROC",
         plot = roc,
         device = "png",
         path = plot_folder,
         width = 12, height = 12, units = "cm")

  rmarkdown::render(template,
                    output_file = paste0(species, ".html"),
                    output_dir = folder,
                    params = list(model = model, test = test),
                    output_options = list(pandoc_args = args),
                    quiet = TRUE
                    )

  html <- paste0(folder, "/", species, ".html")
  browseURL(html)
}
