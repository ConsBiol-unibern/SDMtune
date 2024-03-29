% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SDMmodel2MaxEnt.R
\name{SDMmodel2MaxEnt}
\alias{SDMmodel2MaxEnt}
\title{SDMmodel2MaxEnt}
\usage{
SDMmodel2MaxEnt(model)
}
\arguments{
\item{model}{\linkS4class{SDMmodel} object to be converted.}
}
\value{
The converted dismo \linkS4class{MaxEnt} object.
}
\description{
Converts an \linkS4class{SDMmodel} object containing a \linkS4class{Maxent}
model into a dismo \linkS4class{MaxEnt} object.
}
\examples{
\donttest{# Acquire environmental variables
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)

# Prepare presence and background locations
p_coords <- virtualSp$presence
bg_coords <- virtualSp$background

# Create SWD object
data <- prepareSWD(species = "Virtual species",
                   p = p_coords,
                   a = bg_coords,
                   env = predictors,
                   categorical = "biome")

# Train a Maxent model
model <- train(method = "Maxent",
               data = data,
               fc = "l")

dismo_model <- SDMmodel2MaxEnt(model)
dismo_model}
}
\author{
Sergio Vignali
}
