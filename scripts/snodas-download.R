#'
#' SNODAS model download and raster manipulation functions
#'


### Functions ###############


#Gets the parameter name from the parameter directory
parFromParDir <- function(parDir) snodasMeta$par[match(parDir,snodasMeta$dirName)]
parDirFromPar <- function(par) snodasMeta$dirName[match(par,snodasMeta$par)]


datFileToRaster <- function(datFileName, par){
  #Performs operations required to read in SNODAS dat file to 
  #Read in binary data
  dataScaleFactor <- snodasMeta$scaleFactor[snodasMeta$par==par]
  
  rasCon <- gzcon(file(datFileName,"rb"))
  rasData <- readBin(rasCon, integer(), n=nRow*nCol, size=2,
                     signed=TRUE, endian='big')/dataScaleFactor
  close(rasCon)
  
  #assign data to raster
  r <- raster(x = matrix(rasData, nrow=nRow, ncol=nCol, byrow = T))
  
  #Assign extents, crs
  extent(r) <- extent(c(ulxmap, llxmap, llymap,  ulymap) )
  crs(r) <- snodasCRS
  r
}

readUntarredSNODAS <- function(unpackedDir,parameters){
  #Inputs are 1) the fully qualified directory path to an untarred 
  #  SNODAS dataset for a single day, and 2) the SNODAS
  #  parameters to read.  See snodasMeta$par for exact names
  r <- list()
  dateTag <- right(unpackedDir,8)
  for(par in parameters){
    rasFileTemplate <- snodasMeta$fileTemplate[snodasMeta$par==par]
    rasFile <- sprintf(sprintf("%s/%s",unpackedDir,rasFileTemplate), dateTag)
    r[[par]] <- datFileToRaster(rasFile, par)
  }
  r
}

convertRasterToInchesFromMeters <- function(ras){
  #https://nsidc.org/data/g02158
  #SWE, snow depth, sublimination in meters
  ras[ras == -0.09999] <- NA  #Setting NA; scale factor of 100,000
  ras[ras == -9.999] <- NA  #Setting NA; scale factor of 1,000
  ras <- ras*1000/25.4     #to inches
  ras
}

#assume 1000kg/m^3 of water
#[kg/m^2] = [kg/m^2]*[1m^3/1000kg][1000mm/m] -> [m]
convertRasterToInchesFromKGM2 <- function(ras){
  #https://nsidc.org/data/g02158
  #precipitation in kg/m^2
  ras[ras == -999.9] <- NA  #Setting NA; used scale factor of 10
  ras <- ras/25.4     #to inches from kg/m^2
  ras
}

convertRasterToDegF <- function(ras){
  #https://nsidc.org/data/g02158
  #snow pack temp in Kelvin
  ras[ras == -9999] <- NA      #Setting NA; used scale factor of 1
  ras <- (ras-273.15)*9/5 + 32 #Conversion to deg F from Kelvin
  ras
}

cleanSNODASRasterList <- function(rList){
  #Performs unit conversions and removal of undefined grid cells in SNODAS raster
  #  datasets.  Input is a list of raster objects, as returned from
  #  the 'dlSNODAStoRaster' function
  for( rName in names(rList)){
    if(rName %in% 
       c("swe",
         "snow depth",
         "sublimination from the snow pack",
         "sublimination from blowing snow" ))
      rList[[rName]] <- convertRasterToInchesFromMeters(rList[[rName]])
    if(rName %in% c("solid precipitation", "liquid precipitation"))
      rList[[rName]] <- convertRasterToInchesFromKGM2(rList[[rName]])
    if(rName %in% c("snow pack average temperature"))
      rList[[rName]] <- convertRasterToDegF(rList[[rName]])
  }
  rList
}

dlSNODAStoRasterList <- function(date,parameters, cleanRaster=T){
  
  date <- as.Date(date, origin = "1970-01-01")
  dateTag <- format(date,"%Y%m%d") #Creating a tag that will be used in the ftp address
  
  ###___Retrieving Files from FTP ###########################
  #Creating urls/directory references for files
  tarFileName <- sprintf("SNODAS_%s.tar", dateTag)
  
  tarURL <- sprintf("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/%g/%s/%s",
                    year(date), monthLabels[month(date)], tarFileName)
  
  
  ###___Downloading SNODAS tar file ####################
  
  dlFilePath <- paste0(tempDir,"/",tarFileName) #download location
  dateExists <- F; attempts <- 1 #initializing loop parameters
  while( !dateExists ){
    tryUrl <- try(expr=download.file(url=tarURL, destfile = dlFilePath, quiet = F, mode="wb"))
    if( class(tryUrl) != "try-error" ) dateExists <- T
    if(attempts > maxDlAttempts) break
  }
  
  #If data doesn't exist for that date, then removing the generated file and continuing
  #  to next iteration
  if(!dateExists){file.remove(dlFilePath); return(NA) }
  
  #If statement for if date exists in SNODAS FTP
  if( dateExists ){
    
    #Establishing directory for where to untar the results
    unpackedDir <- paste0(tempDir,"/",gsub(".tar","",tarFileName))
    tarError <- tryCatch(expr= untar(tarfile=dlFilePath,exdir = unpackedDir),
                         error=function(e) return(T))
    
    file.remove(dlFilePath) #Getting rid of data download
    #Because there are occassionally errors with the download, this code will
    #  then check that the tar file can actually be read without breaking the 
    #  foreach loop
    if( class(tarError) == "try-error" ){unlink(unpackedDir,recursive = T); return(NA)}
    
    r <- readUntarredSNODAS(unpackedDir,parameters)
    
    unlink(unpackedDir,recursive = T)
    if(cleanRaster) r <- cleanSNODASRasterList(r)
    
  } #End date exists if
  r
}

saveRasters <- function(r,date){
  #Iterate through each list element
  #save to a folder corresponding to same name as list element (e.g., 'swe')
  #  in 'saveDir'.  Save file name is prefixed with parameter name
  for(par in names(r)){
    dirName <- snodasMeta$dirName[snodasMeta$par==par] #directory name
    parSaveDir <- sprintf("%s\\%s",saveDir,dirName)    #full directory path
    if(!dir.exists(parSaveDir)) dir.create(parSaveDir,recursive = T) #creating directory if needed
    tifFile <- sprintf("%s_%s.tif",dirName,format(date,"%Y-%m-%d"))
    saveFileName <- sprintf("%s/%s",parSaveDir,tifFile) #full save file path
    writeRaster(x=r[[par]], filename=saveFileName,overwrite=TRUE) #saving to raster
  }
}

dlSNODAStoRasterList <- function(date,parameters, cleanRaster=T){
  
  date <- as.Date(date, origin = "1970-01-01")
  dateTag <- format(date,"%Y%m%d") #Creating a tag that will be used in the ftp address
  
  ###___Retrieving Files from FTP ###########################
  #Creating urls/directory references for files
  tarFileName <- sprintf("SNODAS_%s.tar", dateTag)
  
  tarURL <- sprintf("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/%g/%s/%s",
                    year(date), monthLabels[month(date)], tarFileName)
  
  
  ###___Downloading SNODAS tar file ####################
  
  dlFilePath <- paste0(tempDir,"/",tarFileName) #download location
  dateExists <- F; attempts <- 1 #initializing loop parameters
  while( !dateExists ){
    tryUrl <- try(expr=download.file(url=tarURL, destfile = dlFilePath, quiet = F, mode="wb"))
    if( class(tryUrl) != "try-error" ) dateExists <- T
    if(attempts > maxDlAttempts) break
  }
  
  #If data doesn't exist for that date, then removing the generated file and continuing
  #  to next iteration
  if(!dateExists){file.remove(dlFilePath); return(NA) }
  
  #If statement for if date exists in SNODAS FTP
  if( dateExists ){
    
    #Establishing directory for where to untar the results
    unpackedDir <- paste0(tempDir,"/",gsub(".tar","",tarFileName))
    tarError <- tryCatch(expr= untar(tarfile=dlFilePath,exdir = unpackedDir),
                         error=function(e) return(T))
    
    file.remove(dlFilePath) #Getting rid of data download
    #Because there are occassionally errors with the download, this code will
    #  then check that the tar file can actually be read without breaking the 
    #  foreach loop
    if( class(tarError) == "try-error" ){unlink(unpackedDir,recursive = T); return(NA)}
    
    r <- readUntarredSNODAS(unpackedDir,parameters)
    
    unlink(unpackedDir,recursive = T)
    if(cleanRaster) r <- cleanSNODASRasterList(r)
    
  } #End date exists if
  r
}
