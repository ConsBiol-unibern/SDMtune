## Test environments
* local ubuntu 18.04, R 4.1.2
* win-builder (devel, release and oldrelease)
* Github actions:
  * linux: R devel, release, and oldrel
  * windows: R release
  * mac: R release

## R CMD check results
0 errors | 0 warnings | 1 note

This solves the NOTES in CRAN checks. However, there is a new NOTE for invalid URLs for R devel and release. For what I understood this happens for DOI URLs and should be fixed later in September. I have been asked to submit it anyway.
