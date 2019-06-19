## Test environments
* local ubuntu 18.04 install, R 3.6.0
* win-builder (devel and release)
* Travis-ci R 3.6.0
* AppVeyor-ci R 3.5.3

## R CMD check results
0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Sergio Vignali <sergio.vignali@iee.unibe.ch>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  AICc (22:32)
  MaxEnt (19:36)
  RStudio (18:28)
  SDMs (14:48)
  
These are not mis-spelled words.

## Changes already done following CRAN comments after submission
* Add citations and doi in description file
* Use single quotes for package, software and API names in description file
* Do not capitalize the text "  maximum entropy  "
