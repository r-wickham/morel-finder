#'
#' Downloading NetCDF data from RFC
#'
#' Usage:
# source("scripts/r_setup.R")
# rfcGrids <-
#   downloadGrids(outDir=dirConfig$data$temp) %>%
#   processNetCdfDir() %>%
#   map(.f = loadNWRFCNetCdf)
#' 



#'
#' Returns urls for various data types given the date
#'
#' @param pullDate  Date, the date at which to retrieve data
#' @param dataTypes strings, the data type(s) to pull. Options are:
#'                  QPE, QPF, QTE, QTF
#'                  Q = Quantitative
#'                  P = Precipitation
#'                  T = Temperature
#'                  E = Estimate
#'                  F = Forecast
#' @return string(s) with valid NWRFC grid donwload URL(s)
#'     
getNetCDFUrls <- function(pullDate, dataTypes = c("QPE","QPF","QTE","QTF")){
  library(lubridate)
  pullDate <- as.Date(pullDate)
  yr <- year(pullDate)
  pullDateStr <- format(pullDate, "%Y%m%d")
  # fname = "{data_type}.{date}12.nc.gz"; e.g., QPE.2019020212.nc
  fNameTemplate <- "%s.%s12.nc.gz"
  fname <- sprintf(fNameTemplate, dataTypes, pullDateStr)
  # URL template for inserting strings
  # "https://www.nwrfc.noaa.gov/weather/netcdf/{year}/{date}/{fname}"
  # date = "%Y%m%d"
  urlTemplate <-
    "https://www.nwrfc.noaa.gov/weather/netcdf/%s/%s/%s"
  sprintf(urlTemplate, yr, pullDateStr, fname)
}

#'
#' Downloads grids to a temporary folder for further processing
#' 
#' @param pullDates Date objects defining dates at which to download grids
#' @param dataTypes strings, the data type(s) to pull. Options are:
#'                  QPE, QPF, QTE, QTF
#'                  Q = Quantitative
#'                  P = Precipitation
#'                  T = Temperature
#'                  E = Estimate
#'                  F = Forecast
#' @param outDir    directory to save downloaded grids
#' @return path to output directory
#' 
downloadGrids <- function(pullDates = (Sys.Date()-14):Sys.Date(),
                          dataTypes = c("QPE","QPF","QTE","QTF"),
                          outDir=tempdir()){
  library(purrr)
  library(parallel)
  pullDates <- as.Date(pullDates, origin="1970-01-01")
  urls <- 
    map2(.x = pullDates, .y = list(dataTypes), .f = getNetCDFUrls) %>%
    unlist()
  # Locations to save downloaded files from URLs
  destFiles <- sprintf("%s/%s", outDir, basename(urls))
  # Attempt to download each in parallel
  cl <- makeCluster(detectCores()-1)
  tryCatch({
    mcmapply(FUN = tryDownload, url=urls, destFile=destFiles)
  }, finally = stopCluster(cl))
  outDir
}

#'
#' Wrapper for downloading URL to file
#' 
#' @param url      string, url link
#' @param destFile string, path to download location
#'
tryDownload <- function(url, destFile){
  try(download.file(url, destFile, quiet = T))
}

#'
#'  Processes the tarred NetCDF files to GeoTiff format
#'    given a directory full of NetCDF files (*.nc.gz)
#' 
processNetCdfDir <- function(netCdfDir, outDir){
  library(rgdal)
  library(raster)
  library(R.utils)
  # Pull the tarred file names
  dir(netCdfDir, pattern = "*.nc.gz", full.names = T) %>%
    map_chr(.f = tryGunzip)
}

#'
#' Attempts to use 'gunzip' function on file, returning either blank
#'   character if bad attempt, or the unzipped file name
#'
tryGunzip <- function(zipFileName){
  out <- try(gunzip(zipFileName, remove=T))
  if(is(out, "try-error")) return("")
  out
}



#'
#' Loads a NetCDF file as a raster object given the full 
#'   path to the file name
#'
loadNWRFCNetCdf <- function(netCdfFile){
  library(raster)
  library(ncdf4)
  r <- try({
    r <- raster::raster(netCdfFile)
  crs(r) <- snodasCRS
  r})
  if(is(r, "try-error")) return(NULL)
  r
}
