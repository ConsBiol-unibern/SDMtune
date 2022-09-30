# The function writes messages

    Code
      c <- thinData(rbind(x, x), env = predictors, progress = FALSE)
    Message
      v Removed no NAs and 9000 duplicated locations

---

    Code
      c <- thinData(x, env = predictors, progress = FALSE)
    Message
      v Removed -6507 NAs and no duplicated locations

---

    Code
      c <- thinData(rbind(x, x), env = predictors, progress = FALSE)
    Message
      v Removed -12950 NAs and 2525 duplicated locations

# The function raises errors

    ! Column "A" does not exist
    i Please use the name of the column with the "x" coordinates.

---

    ! Column "B" does not exist
    i Please use the name of the column with the "y" coordinates.

---

    ! `env` must be a <SpatRaster> object
    x You have supplied a <character> instead.

# The function warns if a raster object is used

    ! <raster> objects will not be accepted in future releases
    i SDMtune now uses terra to handle spatial data. See function `terra::rast()` to migrate.

