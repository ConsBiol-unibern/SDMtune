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

## Changes made following CRAN comments after submission
* I cannote remove 'snow' package from the suggested package because it's used by the 'raster' package;
* Add citations and doi in the description file;
* Use single quotes for package, software and API names in thedescription file;
* Do not capitalize the text "... maximum entropy ...";
* Use GPL-3 in the license field instead GPL-3 | file LICENSE and rename the file LICENSE to LICENSE.note;
* Add additionally toy example and use \donttest{} instead of \dontrun{} if examples take more than 5 sec;
