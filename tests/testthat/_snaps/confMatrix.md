# Exception are raised

    Code
      confMatrix(SDMtune:::bm_maxent_cv, type = "cloglog")
    Condition
      Error in `confMatrix()`:
      ! Function available only for <SDMmodel> objects.
      x You have supplied a <SDMmodelCV> instead.

---

    Code
      confMatrix(m, test = "a", type = "cloglog")
    Condition
      Error in `confMatrix()`:
      ! `test` must be an <SWD> object
      x You have supplied a <character> instead.

