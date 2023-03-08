# Exception are raised

    Sum of `keep_best` and `keep_random` cannot be more than 1.

---

    ! You must provide at least two hyperparameters to be tuned
    i Use `gridSearch()` to tune only one parameter.

---

    One hyperparameter in hypers should have more than two values to allow crossover.

---

    ! Number of possible random models is lewer than population size
    i Add more values to the `hyper` argument.

---

    ! Number of possible random models is the same than population size
    i Use `gridSearch()` function.

---

    Optimization algorithm interrupted at generation 0 because it overfits validation dataset.

# The function raises errors

    ! `env` must be a <SpatRaster> object
    x You have supplied a <character> instead.

# The function raises an error if a raster object is used

    ! Objects from the raster package are no longer supported!
    i SDMtune now uses terra to handle spatial data. See function `terra::rast()` to migrate.

