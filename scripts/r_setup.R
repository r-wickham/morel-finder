#'
#' Establishes all packages, configurations, and settings needed to 
#'   execute any functions in this package.
#'

suppressPackageStartupMessages({
  library(purrr)
  library(raster)
  library(snowdl)
  library(yaml)
  library(rgdal)
})

### Startup Helper Functions -------------------------------------------------

"%!in%" <- function(x,y) !(x %in% y) #not in operator

convertType <- function(x){
  # Converts to logical, character, or preserves character
  if(is.character(x)) 
    x <- type.convert(x, as.is=T)
  x
}

loadConfigToGlobal <- function(){
  #' Loads yaml configuration properties for compute, 
  #'   converting to proper type, and exporting variables to global env
  library(yaml)
  cnfg <- read_yaml("config.properties")
  globalList <- rapply(cnfg, convertType, how="list")
  list2env(globalList, .GlobalEnv) # Assign to global env
}

### Load Config --------------------------------------------------------------

loadConfigToGlobal()

### Convert Configs ----------------------------------------------------------

# clipExtentVector just defines the min and max lat/long extents 
#   from a vector in the form of xmin, xmax, ymin, ymax and needs to be
#   converted to an Extent object.  This is done by passing a 2x2 matrix
#   to the extent function in raster package
clipExtent = extent(matrix(clipExtentVector,byrow=T,nrow=2))
