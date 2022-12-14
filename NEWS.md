# SDMtune 1.2.0
* `SDMtune` migrated to package `terra` to handle spatial data. Objects created with the package `raster` will not be supported in future releases
* `SDMtune` has a new progress bar and messages/warnings/errors have a new look
* `modelReport()`, `prepareSWD`, `reduceVariable()`, `thinData()`, and `varSel()` gain the argument `verbose` to disable informative messages
* `gridSearch()`, `optimizeModel()`, `randomSearch()`, `reduceVar()`, and `varSel()` gain the argument `interactive` to disable the interactive real-time chart
* `doJk()`, `gridSearch()`, `optimizeModel()`, `predict()`, `randomSearch()`, `thinData()`, `train()`, `varImp()`, and `varSel()` gain the argument `progress` to disable the progress bar
* The function `thinData()` prints a message with the number of removed locations
* The argument `factors` in `modelReport()` is deprecated. See `terra` documentation to see how to use factors during predictions

Bug fixes:

* Real-time charts are again displayed in Windows
* `plotCor()` plots a correct heatmap when there are more than 9 variables
* `doJk()` runs also without testing dataset

# SDMtune 1.1.6
Resolve CRAN notes

# SDMtune 1.1.5
Bug fixes:

* Allow fold partitions created with `ENMeval` >= 2.0
* Fix error generated when plotting response for categorical predictors using RF
* Update maxnet to version 0.1.4 to solve issue in cran checks

# SDMtune 1.1.4
Bug fixes: Add the argument `factors` to the function `modelReport()` to allow predictions for raster objects including categorical variables

# SDMtune 1.1.3
Include Java >=8 in system requirements

# SDMtune 1.1.2

* Removed deprecated function `get_tunable_args()`.
* Removed deprecated argument "parallel" form functions.
* Updated citation text

# SDMtune 1.1.1
Main changes:

* New function `checkMaxentInstallation()` to check if Maxent is correctly configured.
* The argument parallel is deprecated and not used anymore. This because it improved computation only for very large datasets and used the superseded package snow.
* The function `get_tunable_args()` has been replaced by `getTunableArgs()` to be consistent with the camel case function naming. At the moment is still possible to use both functions but `get_tunable_args()` will be deprecated in the next release.
* Four new vignette are now available with the package.

Bug fix: `plotPA` function now works also with new version of `ggplot2`.

# SDMtune 1.1.0
Main changes:

* The function `thinData` accept now a matrix or a dataframe with several columns, useful if the users has information related to the coordinates that doesn't want to lose with the thinning procedure.
* The function `plotResponse` plots the response for the full range of presences and backgrounds/absences when only_presence is TRUE, only_presence is unused only to compute the provided function to the range of presence locations when marginal = TRUE

Bug fix:

* Interactive plot of SDMtune objects are again displayed in the RStudio Viewer pane

# SDMtune 1.0.1
Bug fixes:

* Fix CRAN errors
* Fix bug introduced with version 1.0.0

# SDMtune 1.0.0
Main changes:

* In this release all deprecated functions and functions' arguments have been removed, including the functions to convert old objects (created with version < 0.2.0.) into the new format.
* Add function `addSamplesToBg` to add presence locations to background location.
* `extra_args` in Maxent models cannot be changed anymore.

Bug fix: Fix CRAN error for r-oldrel

# SDMtune 0.2.1
Main changes:

* New function to predict `SDMmodelCV` objects
* Add the possibility to pass multiple methods to the `train` function
* Add possibility to compute the testing AUC or TSS for a held apart testing dataset in the case of a `SDMmodelCV` object
* Add the possibility to merge only the presence locations in the `mergeSWD` function

Bug fixes:

* `plotPA` function now works also with large raster objects 
* Removed font family from plot functions to avoid errors when the font is not available

# SDMtune 0.2.0
This release is the first step to enable more methods to train models. The main change is that the `SDW` object now bundles together the presence and the absence/background locations and is not necessary anymore to pass the presence and absence locations as separate arguments to the `train` function.

Old objects of class `SWD`, `SDMmodel`, `SDMmodelCV` and `SDMtune` created with version <= 0.1.1 must be converted into the new format using the dedicated help functions. 

Main changes:

* The `SWD` object bundles together the presence and absence/background locations
* New methods to train models: Artificial Neural Network (ANN), Boosted Regression Trees (BRT) and Random Forest (RF)
* Enabled spatial cross validation: the function `train` accepts now folds partition generated with other packages (i.e. ENMeval and blockCV)
* Add title argument in `plot` function for `SDMtune` objects
* New randomly generated dataset `virtualSp` with presence, absence and background locations
* Number of background locations cannot be tuned anymore using the tuning functions

Deprecated function: `getSubsample`

# SDMtune 0.1.1

* Fix bug in `VarImp` function for `SDMmodelCV` objects
* Real-time charts are now displayed also when R is not executed from RStudio
* `maxentVarImp` function available for `SDMmodelCV` objects

# SDMtune 0.1.0

* First release
