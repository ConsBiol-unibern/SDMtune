# Exception are raised

    Code
      optimizeModel(mother, h, "auc", data, keep_best = 0.6, keep_random = 0.6, pop = 3)
    Condition
      Error in `.check_optimize_args()`:
      ! Sum of `keep_best` and `keep_random` cannot be more than 1.

---

    Code
      optimizeModel(mother, list(fc = "l"), "auc", data)
    Condition
      Error in `.check_optimize_args()`:
      ! You must provide at least two hyperparameters to be tuned
      i Use `gridSearch()` to tune only one parameter.

---

    Code
      optimizeModel(mother, list(fc = "l", reg = 1), "auc", data)
    Condition
      Error in `.check_optimize_args()`:
      ! One hyperparameter in hypers should have more than two values to allow crossover.

---

    Code
      optimizeModel(mother, h, "auc", data, pop = 7)
    Condition
      Error in `.check_optimize_args()`:
      ! Number of possible random models is lewer than population size
      i Add more values to the `hyper` argument.

---

    Code
      optimizeModel(mother, h, "auc", data, pop = 6)
    Condition
      Error in `.check_optimize_args()`:
      ! Number of possible random models is the same than population size
      i Use `gridSearch()` function.

---

    Code
      optimizeModel(mother, h, "auc", data, pop = 3)
    Condition
      Error in `optimizeModel()`:
      ! Optimization algorithm interrupted at generation 0 because it overfits validation dataset.

