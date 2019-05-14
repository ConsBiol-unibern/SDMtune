.create_chart <- function(folder, script, settings, data, height = 300) {

  dir.create(folder)

  # Copy libraries and style files in lib directory
  dir.create(file.path(folder, "lib"))
  files <- list.files(system.file("lib", package = "SDMtune"),
                      full.names = TRUE)
  file.copy(files, file.path(folder, "lib"))

  # Copy chart template
  file.copy(file.path(system.file("templates", package = "SDMtune"),
                      "chart_template.html"),
            folder)

  # render script
  .render_script(folder, script, settings, data)

  path <- file.path(folder, "chart_template.html")
  viewer <- getOption("viewer")
  # Show chart in viewer pane if not called from testthat
  if (!is.null(viewer) & !Sys.getenv("TESTTHAT") == "true") {
    viewer(path, height = height)
  }
  Sys.sleep(.1)
}

#' @importFrom jsonlite toJSON
#' @importFrom whisker whisker.render
.render_script <- function(folder, script, settings, data) {

  template <- paste(readLines(file.path(system.file("scripts",
                                                    package = "SDMtune"),
                                        script),
                              encoding = "UTF-8"),
                    collapse = "\n")

  data <- list(settings = jsonlite::toJSON(settings),
               data = jsonlite::toJSON(data))

  rendered_script <- whisker::whisker.render(template, data = data)
  writeLines(rendered_script, file.path(folder, "lib", "chart_script.js"))
}

#' @importFrom jsonlite write_json
.update_data <- function(folder, data) {
  jsonlite::write_json(data, file.path(folder, "data.json"))
  Sys.sleep(.1)
}
