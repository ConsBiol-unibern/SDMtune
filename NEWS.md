# SDMtune 0.2.0
This release is the first step to enable more methods to train models. The main change is that the `SDW` object now bundles together the presence and the absence/background locations and is not necessary anymore to pass the presence and absence locations as separate arguments to the `train` function.

Old objects of class `SWD`, `SDMmodel`, `SDMmodelCV` and `SDMtune` created with version <= 0.1.1 must be converted into the new format using the dedicated help functions. Please check the article [Deprecated objects](https://consbiol-unibern.github.io/SDMtune/articles/articles/deprecated-objects.html) in the package website.  

Main changes:

* The `SWD` object bundles together the presence and absence/background locations
* New methods to train models: Artificial Neural Network (ANN), Boosted Regression Trees (BRT) and Random Forest (RF)
* Enabled spatial cross validation: the function `train` accepts now folds partition generated with other packages (i.e. ENMeval and blockCV)
* Add title argument in `plot` function for `SDMtune` objects
* New randomly generated dataset `virtualSp` with presence, absence and background locations
* Number of background locations cannot be tuned anymore using the tuning functions

Deprecated function:

* `getSubsample`

# SDMtune 0.1.1
* Fix bug in `VarImp` function for `SDMmodelCV` objects
* Real-time charts are now displayed also when R is not executed from RStudio
* `maxentVarImp` function available for `SDMmodelCV` objects

# SDMtune 0.1.0
* First release
