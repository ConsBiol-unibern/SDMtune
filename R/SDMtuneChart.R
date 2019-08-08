#' @export
print.SDMtuneChart <- function(x, ...) {
  if (file.exists(x)) {
    .show_chart(x)
  } else {
    # This happen when a new R session is started
    warning("File ", x, " does not exist anymore, please plot the chart again!",
            call. = FALSE)
  }
}


#' Save Chart
#'
#' Save the interactive chart in a html file
#'
#' @param x the output of the \link{plot,SDMtune,missing-method}.
#' @param filename character containing the name of the file in which to save
#' the chart.
#'
#' @details Note that the \code{filemane} argument should be provided without
#' the extension (i.e. "my_chart" and not "my_chart.html").
#'
#' @export
#'
#' @author Sergio Vignali
#'
#' @seealso \link{plot,SDMtune,missing-method}
#'
#' @examples
#' \donttest{
#' # Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
#'                     pattern = "grd", full.names = TRUE)
#' predictors <- raster::stack(files)
#'
#' # Prepare presence and background locations
#' p_coords <- virtualSp$presence
#' bg_coords <- virtualSp$background
#'
#' # Create SWD object
#' data <- prepareSWD(species = "Virtual species", p = p_coords, a = bg_coords,
#'                    env = predictors, categorical = "biome")
#'
#' # Split presence locations in training (80%) and testing (20%) datasets
#' datasets <- trainValTest(data, test = 0.2, only_presence = TRUE)
#' train <- datasets[[1]]
#' test <- datasets[[2]]
#'
#' # Train a model
#' model <- train(method = "Maxnet", data = train, fc = "l")
#'
#' # Define the hyperparameters to test
#' h <- list(reg = 1:5, fc = c("lqp", "lqph"))
#'
#' # Run the gridSearch function using as metric the AUC
#' output <- gridSearch(model, hypers = h, metric = "auc", test = test)
#'
#' # Plot the interactive chart
#' p <- plot(output, title = "My experiment", interactive = TRUE)
#' p
#' # Save the chart in a html file
#' saveChart(p, "my_chart")
#'}
saveChart <- function(x, filename) {

  if (class(x) != "SDMtuneChart")
    stop("Argument x is not of class SDMtuneChart!")

  libs <- c("jquery.min.js", "Chart.min.js")
  html <- readLines(file.path(x, "chart_template.html"))
  # Include libraries
  for (lib in libs) {
    index <- which(grepl(lib, html))
    text <- paste(readLines(file.path(x, "lib", lib)), collapse = " ")
    html[index] <- paste("\t<script>", text, "</script>")
  }
  # include css
  index <- which(grepl("stylesheet", html))
  text <- paste(readLines(file.path(x, "lib", "style.css")), collapse = " ")
  html[index] <- paste("\t<style>", text, "</style>")

  # include script
  script <- "chart_script.js"
  index <- which(grepl(script, html))
  text <- readLines(file.path(x, "lib", script))
  html[index] <- "\t<script>"
  final_html <- c(html[1:index], text, "</script>",
                  html[(index + 1):length(html)])

  # Check if filename contains the extension
  if (!grepl(".html", filename))
    filename <- paste0(filename, ".html")

  file.create(filename)
  writeLines(final_html, filename)
}
