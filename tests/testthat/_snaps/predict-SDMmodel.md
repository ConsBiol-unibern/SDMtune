# The function raises errors

    ! `data` must be an object of class <data.frame>, <SWD> or <SpatRaster>
    x You have supplied a <character> instead.

---

    ! `extent` must be a <SpatExtent> object
    x You have supplied a <character> instead.

---

    x Filename must include the extension

# The function warns

    ! <raster> objects will not be accepted in future releases
    i SDMtune now uses terra to handle spatial data. See function `terra::rast()` to migrate.

---

    ! <Extent> objects will not be accepted in future releases
    i SDMtune now uses terra to handle spatial data. See function `terra::ext()` to migrate.

---

    ! The argument "format" is deprectated and will be ignored. Use "wopt" instead and see "Details" in `terra::writeRaster()`

