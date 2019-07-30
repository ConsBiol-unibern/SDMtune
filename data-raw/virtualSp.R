library(virtualspecies)

# Load predictors from dismo pkg
files <- list.files(path = paste0(system.file(package = "dismo"), "/ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

set.seed(516516)
virtual_sp <- generateRandomSp(predictors)
virtual_sp <- convertToPA(virtual_sp, beta = 0.51, alpha = -0.1)

locs <- sampleOccurrences(virtual_sp, n = 1000, type = "presence-absence")
virtualSp <- list(presence = locs$sample.points[locs$sample.points$Real == 1,
                                                1:2],
                  absence = locs$sample.points[locs$sample.points$Real == 0,
                                               1:2])
bg <- dismo::randomPoints(predictors[[9]], 5000)
virtualSp$background <- as.data.frame(bg)
virtualSp$pa_map <- virtual_sp$pa.raster
virtualSp$prob_map <- virtual_sp$probability.of.occurrence

# Save object in data folder
usethis::use_data(virtualSp, overwrite = TRUE)
