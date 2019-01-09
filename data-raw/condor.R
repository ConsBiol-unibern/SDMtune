# Load predictors from dismo pkg
files <- list.files(path = paste0(system.file(package = "dismo"), "/ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

# Download condor data collected from 2008 to 2018 from GBIF database
condor_gbif <- dismo::gbif(genus = 'Vultur', species = 'gryphus*', geo = TRUE,
                           removeZeros = TRUE, args = 'year=2008,2018')
condor_raw <- data.frame(x = condor_gbif$lon, y = condor_gbif$lat,
                         key = condor_gbif$key,
                         datasetKey = condor_gbif$datasetKey,
                         stringsAsFactors = FALSE)
condor_raw <- condor_raw[!duplicated(condor_raw), ]  # Remove duplicate rows
condor_raw <- condor_raw[complete.cases(condor_raw), ]  # Remove NA values

# Thin data, same than thinData function but adapted to preserve the observation
# key and the dataset key

# Set seed to get always same data
set.seed(25)
cells <- raster::cellFromXY(predictors, condor_raw[, 1:2])
unique_cells <- unique(cells)
# Remove cells where coords are NA
cells <- cells[complete.cases(raster::extract(predictors, condor_raw[, 1:2]))]

condor <- data.frame(x = double(), y = double(), key = character(),
                     datasetKey = character(), stringsAsFactors = FALSE)
for (i in 1:length(unique_cells)) {
  if (length(which(cells == unique_cells[i])) > 1) {
    # Sample duplicates
    sample <- sample(nrow(condor_raw[cells == unique_cells[i], ]), 1)
    condor[i, ] <- unlist(condor_raw[sample, ])
  } else {
    condor[i, ] <- unlist(condor_raw[i, ])
  }
}
# Convert columns to the right data type, the list function coerces all to
# character
condor$x <- as.numeric(condor$x)
condor$y <- as.numeric(condor$y)
condor$key <- as.integer(condor$key)

# Collect citations for data documentation
citations <- c()
for (key in unique(condor$datasetKey)) {
  citations <- c(citations, rgbif::gbif_citation(key)$citation$citation)
}

# Check for rights
rights <- c()
for (key in unique(condor$datasetKey)) {
  rights <- c(rights, rgbif::gbif_citation(key)$right)
}

# Save object in data folder
usethis::use_data(condor, overwrite = TRUE)

# Extract envaronmental condition at presence locations
p <- prepareSWD(species = "Vultur gryphus", coords = condor[, 1:2],
                env = predictors, categoricals = "biome")

# Extract 9100 bg locations
bg <- dismo::randomPoints(predictors, 9100)
bg <- prepareSWD(species = "Vultur gryphus", coords = bg,
                 env = predictors, categoricals = "biome")
# Take the first 9000 location, the previous function remove some NAs
bg@data <- bg@data[1:9000, ]
bg@coords <- bg@coords[1:9000, ]
# save them as SWD objects in sysdata for internal use during testing
usethis::use_data(bg, p, internal = TRUE, overwrite = TRUE)
