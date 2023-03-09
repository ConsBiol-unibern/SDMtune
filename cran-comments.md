## Test environments
* local ubuntu 22.04, R 4.2.2
* Github actions:
  * linux: R devel, release, and oldrel
  * windows: R release
  * mac: R release
* win-builder (devel, release and oldrelease)

Win builder generates 1 NOTE:

* R oldrelease and devel: "Possibly mis-spelled words in DESCRIPTION: SDMs". This is not a mis-spelled word, it is just the acrimonious for "Species Distribution Models" and it is included in the WORDLIST file.

* R release: "Found the following (possibly) invalid URLs" but they are all valid DOI urls
