.onAttach <- function(libname, pkgname) {
  packageStartupMessage(" ________________________________________")
  packageStartupMessage("|   ____   ____   __  __             _   |")
  packageStartupMessage("|  / ___| |  _ \\ |  \\/  | ___   ___ | |  |")
  packageStartupMessage("|  \\___ \\ | | | || |\\/| |/ __| / _ \\| |  |")
  packageStartupMessage("|   ___) || |_| || |  | |\\__ \\|  __/| |  |")
  packageStartupMessage("|  |____/ |____/ |_|  |_||___/ \\___||_|  |")
  packageStartupMessage("|________________________________________|")
  packageStartupMessage("")
}
.sdmsel <- new.env(parent = emptyenv())
.sdmsel$jQuery <- paste(readLines(system.file("templates/library",
                                              "jQuery.js",
                                              package = "SDMsel"),
                                  encoding = "UTF-8"), collapse = "\n")
.sdmsel$chartJs <- paste(readLines(system.file("templates/library",
                                               "Chart.bundle.js",
                                               package = "SDMsel"),
                                   encoding = "UTF-8"), collapse = "\n")
.sdmsel$optimiseCss <- paste(readLines(system.file("templates",
                                                   "optimiseModel.css",
                                                   package = "SDMsel"),
                             encoding = "UTF-8"), collapse = "\n")
.sdmsel$optimiseTemplate <- paste(readLines(system.file("templates",
                                                        "optimiseModel.html",
                                                        package = "SDMsel"),
                                   encoding = "UTF-8"), collapse = "\n")
