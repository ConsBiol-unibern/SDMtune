# Load predictors from dismo pkg
library(zeallot)
files <- list.files(path = paste0(system.file(package = "dismo"), "/ex"),
                    pattern = "grd", full.names = TRUE)
predictors <- raster::stack(files)

# Extract envaronmental condition at presence locations
data <- prepareSWD(species = "Vultur gryphus", coords = condor[, 1:2],
                   env = predictors, categoricals = "biome")
c(p, t) %<-% trainValTest(data, .2)

# Extract 9100 bg locations
set.seed(25)
bg <- dismo::randomPoints(predictors, 10000)
bg <- prepareSWD(species = "Vultur gryphus", coords = bg,
                 env = predictors, categoricals = "biome")

# Get subsample to train models
bg_model <- getSubsample(bg, size = 5000, seed = 25)

# Train base models with default settings
bm_maxent <- train("Maxent", p, bg_model)
bm_maxent_cv <- train("Maxent", p, bg_model, rep = 4)
bm_maxnet <- train("Maxnet", p, bg_model)

# save objects in sysdata
usethis::use_data(p, t, bg, bm_maxent, bm_maxent_cv, bm_maxnet, internal = TRUE,
                  overwrite = TRUE)
