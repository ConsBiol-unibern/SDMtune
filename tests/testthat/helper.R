create_local_chart <- function(folder, script, update, env = parent.frame()) {
  settings <- list(update = FALSE)
  data <- list()
  .create_chart(folder = folder, script = script, settings = settings,
                data = data)
  if (update)
    .update_data(folder, data = settings)
  withr::defer(unlink(folder, recursive = TRUE), envir = env)
}

create_local_model_report <- function(folder, env = parent.frame()) {

  if (file.exists(file.path(getwd(), folder)))
    unlink(file.path(getwd(), folder), recursive = TRUE)

  m <- SDMtune:::bm_maxnet
  files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                      pattern = "grd", full.names = TRUE)
  env_vars <- raster::stack(files)

  modelReport(m, type = "cloglog", folder = folder, test = SDMtune:::t,
              permut = 1, env = env_vars, verbose = FALSE)

  withr::defer(unlink(file.path(getwd(), folder), recursive = TRUE),
               envir = env)
}
