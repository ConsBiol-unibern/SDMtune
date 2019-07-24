# Load predictors from dismo pkg
library(zeallot)
files <- list.files(path = paste0(system.file(package = "dismo"), "/ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

# Presence locations
p <- condor[, 1:2]

# Bg locations
set.seed(25)
bg <- dismo::randomPoints(predictors[[9]], 5000)

# Create train dataset, don't use the variable name train because in testthat
# is interpreted as the train function when using SDMtune:::train
t <- prepareSWD(species = "Vultur gryphus", p = p, a = bg, env = predictors,
                categorical = "biome")

folds <- randomFolds(data = t, k = 4, only_presence = TRUE)

# Train base models with default settings
# Maxent
bm_maxent <- train("Maxent", data = t)
bm_maxent_cv <- train("Maxent", data = t, folds = folds)

# Maxnet
bm_maxnet <- train("Maxnet", data = t)
bm_maxnet_cv <- train("Maxnet", data = t, folds = folds)

# Feature Class mapping
fc_map = list(
  "l" = "nolinear",
  "q" = "noquadratic",
  "p" = "noproduct",
  "h" = "nohinge"
)

# save objects in sysdata
usethis::use_data(t, bm_maxent, bm_maxent_cv, bm_maxnet, bm_maxnet_cv, fc_map,
                  internal = TRUE, overwrite = TRUE)
