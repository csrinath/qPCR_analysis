# Loads package "x". If not present in library, installs and then loads "x".
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Install pacman (which has better Library functions)
if (!require("pacman")) install.packages("pacman")

# New packages can be loaded/installed+loaded using pacman::p_load(package1, package2, package_n)

