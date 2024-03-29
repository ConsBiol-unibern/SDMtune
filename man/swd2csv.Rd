% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swd2csv.R
\name{swd2csv}
\alias{swd2csv}
\title{SWD to csv}
\usage{
swd2csv(swd, file_name)
}
\arguments{
\item{swd}{\linkS4class{SWD} object.}

\item{file_name}{character. The name of the file in which to save the object,
see details.}
}
\description{
Save an \linkS4class{SWD} object as csv file.
}
\details{
\itemize{
\item The \code{file_name} argument should include the extension (i.e. my_file.csv).
\item If \code{file_name} is a single name the function saves the presence
absence/background locations in a single file, adding the column \strong{pa} with
1s for presence and 0s for absence/background locations. If \code{file_name} is a
vector with two names, it saves the object in two files: the first name
is used for the presence locations and the second for the absence/background
locations.
}
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
\dontrun{
# The following commands save the output in the working directory
# Save the SWD object as a single csv file
swd2csv(data,
        file_name = "train_data.csv")

# Save the SWD object in two separate csv files
swd2csv(data,
        file_name = c("presence.csv", "absence.csv"))}}
}
\author{
Sergio Vignali
}
