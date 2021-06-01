#'
#' Test downloading latest GeoJSON from Morel Finder
#'


urlTheGreatMorel <- 
  paste0("https://www.thegreatmorel.com/maps/geojson/",
         "layer/31,32,33,34,35,36/?callback=jsonp&full=yes&full_icon_url=yes")


# download.file(url = urlTheGreatMorel, destfile = "D:/temp/test.json")


### Attemp 1 rjson -----------------------------------------------------------
library(rjson)


f = file(urlTheGreatMorel)
j <- rjson::fromJSON(file = f)
j <- fromJSON(file = urlTheGreatMorel)
close(f)


### Attempt 2 jsonlite -------------------------------------------------------

library(jsonlite)

j <- jsonlite::fromJSON(txt = urlTheGreatMorel)

