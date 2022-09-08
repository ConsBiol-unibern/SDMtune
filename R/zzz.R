text <- paste0("
   _____  ____   __  ___ __
  / ___/ / __ \\ /  |/  // /_ __  __ ____   ___
  \\__ \\ / / / // /|_/ // __// / / // __ \\ / _ \\
 ___/ // /_/ // /  / // /_ / /_/ // / / //  __/
/____//_____//_/  /_/ \\__/ \\__,_//_/ /_/ \\___/  version ",
  utils::packageVersion("SDMtune"),
  "\n\nTo cite this package in publications type: citation(\"SDMtune\").")


.onAttach <- function(libname,
                      pkgname) {

  packageStartupMessage(text)
}
