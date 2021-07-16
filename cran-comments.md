## Test environments
* local ubuntu 18.04 install, R 3.6.3
* win-builder (devel, release and oldrelease)
* Github actions:
  * linux: R devel, release, and oldrel
  * windows: R release
  * mac: R release

## R CMD check results
0 errors | 0 warnings | 0 notes

This solves the error in donttest. I could not reproduce the error for macos with arm architecture. From the message it looks like a crash given that the check stops after an if statement without any error message.
