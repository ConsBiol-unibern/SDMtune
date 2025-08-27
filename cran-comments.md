## Test environments
* local Windows, R 4.5.1
* Github actions:
  * linux: R devel, release
  * windows: R release
  * mac: R release, oldrel
* win-builder (devel, release and oldrel)
* macOS builder

This release is to update tests to be compatible with ggplot2 version 4.0.0.

Win builder (oldrel) generates 1 NOTE:

"Found the following (possibly) invalid URLs" but they are both valid urls.
