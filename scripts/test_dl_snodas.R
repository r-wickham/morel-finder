#'
#' testing download and processing SNODAS
#'

source("scripts/r_setup.R")


download_SNODAS(as.Date("2021-05-25"),
                out_dir = dirConfig$data$tar,
                overwrite = FALSE)

# Unpacks all SNODAS data - not just SWE and depth
unpack_SNODAS(tar_dir = dirConfig$data$tar,
              out_dir = dirConfig$data$raw,
              rm_tar = TRUE)

# Could borrow heavily from this function and
#  modify to write out specific rasters
#   Currently overwrites SWE with depth b/c only saving
#   out to one file name
rFileNames <- rasterize_SNODAS(
  data_dir = dirConfig$data$raw,
  out_dir = dirConfig$data$temp,
  rm_data = TRUE,
  format = "GTiff",
  crop = clipExtent,
  reproject = NULL,
  method = "ngb",
  verbose = TRUE
)

r <- raster(x = rFileNames)

plot(r)

rgdal::readGDAL(fname = paste0("D:\\gitRepo\\morel-finder\\data\\raster",
                               "\\raw\\SNODAS_20210525\\",
                               "us_ssmv01025SlL00T0024TTNATS2021052505DP001.dat"),)


r <- raster(x = "data/raster/raw/SNODAS_20210525/us_ssmv01025SlL00T0024TTNATS2021052505DP001.dat")

