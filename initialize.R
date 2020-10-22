options(dplyr.summarise.inform = FALSE)

#
# Ensure required libraries are available
#
ensureLibraries <- function() {
  if (!require(data.table)) {
    stop("The data.table package is required, but the package does not exist.")
  }
  
  if (!require(dplyr)) {
    stop("The dplyr package is required, but the package does not exist.")
  }
  
  if (!require(tidyr)) {
    stop("The tidyr package is required, but the package does not exist.")
  }
  
  if (!require(stringr)) {
    stop("The stringr package is required, but the package does not exist.")
  }
  
  if (!require(readr)) {
    stop("The readr package is required, but the package does not exist.")
  }
  
  if (!require(xlsx)) {
    stop("The readr package is required, but the package does not exist.")
  }
}

ensureLibraries()

