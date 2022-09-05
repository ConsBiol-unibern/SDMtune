# The function raises errors

    Code
      mergeSWD(t, t@data)
    Condition
      Error in `mergeSWD()`:
      ! The function accepts only <SWD> objects.

---

    Code
      mergeSWD(x, t)
    Condition
      Error in `mergeSWD()`:
      ! `swd1` and `swd2` have a different species!

# The function warns if datasets have different variables

    The two SWD objects have different columns, only the common columns are used in the merged object

