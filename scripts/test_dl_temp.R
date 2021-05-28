#'
#' Testing pulling temperature data using the 'snowdl' package
#'

library(snowdl)

install.packages("daymetr")
library(daymetr)


tempGrds <- 
  download_daymet_ncss(location = clipExtentVector[c(4,1,2,2)],
                       start = 2020,
                       end = 2020,
                       param=c("tmin","tmax"),
                       frequency = "daily")





