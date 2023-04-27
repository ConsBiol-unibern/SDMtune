# The function .check_args function raises exceptions

    You must provide the `env` argument if you want to use the AICc

---

    AICc not allowed with <SDMmodelCV> objects

---

    You need to provide a test dataset

---

    Argumnt lambda non included in tunable hyperparameters

---

    Argumnts beta and lambda non included in tunable hyperparameters

# The function .get_method gives the right output

    Code
      .get_method(SDMtune:::bm_maxent)
    Output
      [1] "Maxent"

---

    Code
      .get_method(SDMtune:::bm_maxnet)
    Output
      [1] "Maxnet"

---

    Code
      .get_method(m)
    Output
      [1] "Artificial Neural Networks"

---

    Code
      .get_method(m)
    Output
      [1] "Boosted Regression Trees"

---

    Code
      .get_method(m)
    Output
      [1] "Random Forest"

---

    Code
      .get_method(SDMtune:::bm_maxnet_cv)
    Output
      [1] "Maxnet"

# Raises and error if raster package is used

    ! Objects from the raster package are no longer supported!
    i SDMtune now uses terra to handle spatial data. See function `terra::rast()` to migrate.

