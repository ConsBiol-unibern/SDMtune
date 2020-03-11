## Test environments
* local ubuntu 18.04 install, R 3.6.1
* win-builder (devel, release and oldrelease)
* Travis-ci R 3.6.2 and devel
* AppVeyor-ci R 3.6.3

## R CMD check results
0 errors | 0 warnings | 0 notes

This release should fix the errors and notes in CRAN for r devel

* Uses the superseded package: 'snow (>= 0.4-3)':
I cannote remove 'snow' package from the suggested package because it's used by the 'raster' package;
