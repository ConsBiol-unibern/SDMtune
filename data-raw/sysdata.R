# Load predictors from dismo pkg
files <- list.files(path = paste0(system.file(package = "dismo"), "/ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

# Extract envaronmental condition at presence locations
p <- prepareSWD(species = "Vultur gryphus", coords = get("condor")[, 1:2],
                env = predictors, categoricals = "biome")

# Extract 9100 bg locations
bg <- dismo::randomPoints(predictors, 9100)
bg <- prepareSWD(species = "Vultur gryphus", coords = bg,
                 env = predictors, categoricals = "biome")
# Take the first 9000 location, the previous function remove some NAs
bg@data <- bg@data[1:9000, ]
bg@coords <- bg@coords[1:9000, ]

# save objects in sysdata
usethis::use_data(p, bg, internal = TRUE, overwrite = TRUE)
