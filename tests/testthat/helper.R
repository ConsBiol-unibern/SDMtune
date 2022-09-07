create_local_chart <- function(folder,
                               script,
                               update,
                               env = parent.frame()) {

  settings <- list(update = FALSE)
  data <- list()
  .create_chart(folder = folder, script = script, settings = settings,
                data = data)
  if (update)
    .update_data(folder, data = settings)
  withr::defer(unlink(folder, recursive = TRUE), envir = env)
}
