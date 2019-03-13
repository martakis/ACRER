
#' Install_Required_Packages
#' Create a function to check for installed packages and install them if they are not already installed.
#' @param packages The required packages
#' @return InstallPackages Install the required packages


InstallPackages <- function(packages) {
  new.packages <- packages[!(packages %in% utils::installed.packages()[, "Package"])]
  if (length(new.packages))
    utils::install.packages(utils::new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}
