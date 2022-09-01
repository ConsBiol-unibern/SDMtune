# The function .check_args function raises exceptions

    Code
      .check_args(model, metric = "aicc", hypers = h)
    Condition
      Error in `.check_args()`:
      ! You must provide the `env` argument if you want to use the AICc

---

    Code
      .check_args(model_cv, metric = "aicc", hypers = h)
    Condition
      Error in `.check_args()`:
      ! AICc not allowed with <SDMmodelCV> objects

---

    Code
      .check_args(model, metric = "auc", hypers = h)
    Condition
      Error in `.check_args()`:
      ! You need to provide a test dataset

---

    Code
      .check_args(model, "auc", data, hypers = h)
    Condition
      Error in `.check_args()`:
      ! Argumnt lambda non included in tunable hyperparameters

---

    Code
      .check_args(model, "auc", data, hypers = h)
    Condition
      Error in `.check_args()`:
      ! Argumnts beta and lambda non included in tunable hyperparameters

