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
# Remove duplicates
condor_raw <- condor_raw[!duplicated(condor_raw[, 1:2]), ]
# Remove NA values
condor_raw <- condor_raw[complete.cases(condor_raw[, 1:2]), ]
# Reset row names
row.names(condor_raw) <- NULL

# Thin data, same than thinData function but adapted to preserve the observation
# key and the dataset key

# Set seed to get always same data
set.seed(25)
cells <- raster::cellFromXY(predictors, condor_raw[, 1:2])
# Remove cells where coords are NA
cells <- cells[complete.cases(raster::extract(predictors, condor_raw[, 1:2]))]
unique_cells <- unique(cells)

condor <- data.frame(x = double(), y = double(), key = character(),
                     datasetKey = character(), stringsAsFactors = FALSE)
for (i in 1:length(unique_cells)) {
  if (length(which(cells == unique_cells[i])) > 1) {
    # Sample duplicates
    sample <- sample(
      as.integer(row.names(condor_raw[cells == unique_cells[i], ])), 1)
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

# Remove duplicates
condor <- condor[!duplicated(condor[, 1:2]), ]

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
