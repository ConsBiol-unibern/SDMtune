# The function raises errors

    ! Column "A" does not exist
    i Please use the name of the column with the "x" coordinates.

---

    ! Column "B" does not exist
    i Please use the name of the column with the "y" coordinates.

---

    ! `env` must be a <SpatRaster> object
    x You have supplied a <character> instead.

# The function raises an error if a raster object is used

    ! Objects from the raster package are no longer supported!
    i SDMtune now uses terra to handle spatial data. See function `terra::rast()` to migrate.

