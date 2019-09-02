## Test environments
* local ubuntu 18.04 install, R 3.6.0
* win-builder (devel and release)
* Travis-ci R 3.6.0
* AppVeyor-ci R 3.5.3

## R CMD check results
0 errors | 0 warnings | 0 notes

* Uses the superseded package: 'snow (>= 0.4-3)':
I cannote remove 'snow' package from the suggested package because it's used by the 'raster' package;
