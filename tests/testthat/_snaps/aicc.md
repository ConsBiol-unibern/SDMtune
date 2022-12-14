# Raises an error if called with the wrong model method

    ! AICc available only for <Maxent> and <Maxnet> models.
    x You have supplied a <RF> instead.

# The function raises errors

    ! `env` must be a <SpatRaster> object
    x You have supplied a <character> instead.

# The function warns if a raster object is used

    ! <raster> objects will not be accepted in future releases
    i SDMtune now uses terra to handle spatial data. See function `terra::rast()` to migrate.

