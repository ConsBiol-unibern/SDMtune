# SDMtune 0.1.1.9000
This release is the first step to enable more methods to train models. The main change is that the `SDW` objecte now bunbles together the presence and the absence/background locations so that is not necessary anymore to pass the presence and absence locations as separate arguments in the `train` function.

Main changed:

* The `SWD` object bundles together the presence and absence/background locations
* Add title argument in `plot` `SDMtune` object
* The interactive plot of an `SDMtune` object can now be saved in a file

Deprecated function:

* `getSubsample`

# SDMtune 0.1.1
* Fix bug in `VarImp` function for `SDMmodelCV` objects
* Real-time charts are now displayed also when R is not executed from RStudio
* `maxentVarImp` function available for `SDMmodelCV` objects

# SDMtune 0.1.0
* First release
