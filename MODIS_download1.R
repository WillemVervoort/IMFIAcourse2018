require(RCurl)
require(httr)
require(tidyverse)
# MOD16A as example
# FTP server
FTP <- "https://e4ftl01.cr.usgs.gov/MOLT/MOD16A2.006/"
# read in list of folders
items <- try(strsplit(getURL(FTP,ssl.verifypeer = FALSE,dirlistonly = TRUE), "\r*\n")[[1]],silent=TRUE)
items2 <- items[grep("folder.gif",items)]

# split out folder names
dirs2 <- items2 %>%
  str_split("\"", simplify=T)
dirs3 <- dirs2[,6] %>%
  str_sub(1,10)


start.date <- "2000.01.01"
end.date <- "2016.06.30"

##dates
dates=dirs3[dirs3 <= end.date & dirs3 >= start.date]

## locations (h and v bands for satellite)
# Queensland
#h=c(30,31) 
#v=11
# NSW
h = c(29,30)
v = 12

## Get image filenames 
getlist2 <- list()
for(j in 1:length(h)){
  for(i in 1:length(dates)){
  	getlist <- try(strsplit(getURL(paste(FTP,dates[i], "/", sep="")), "\r*\n")[[1]],silent=TRUE)
  	getlist1 <- grep(paste("h",h[j],"v",v,sep=""), getlist)[2]
  	getlist2[[(j-1)*length(dates)+i]] <- getlist[getlist1]
  }
}
# split out the image names from the paths
image1 <- lapply(getlist2, function(x) str_split(x, "\"", simplify = TRUE))
image2 <- na.omit(unlist(lapply(image1, function(x) as.vector(unlist(x))[6])))

# definition of FTP server and working dir
WDIR <- "C:\\Users\\rver4657\\owncloud"
FTP1 <- "https://e4ftl01.cr.usgs.gov/MOLT/MOD44B.006/"

dates2 <- c(dates,dates)

## Download images enter your password and user name
for(i in 1:length(image2)){
	GET(paste(FTP1, dates2[i], image2[i], sep = "/") 
	, write_disk(paste(WDIR, image2[i], sep = "/"), overwrite=TRUE) 
	, authenticate("My_username", "My_password"))
}



## --------------------------------------------------------------------
## reproject images if you want to do this
# custom function to write parameter file for MRT
# The easiest way to get the inputs right for MRT is to check the manual
# and run the GUI on a test example
MRTproj <- function(fname="tmp.prm",hdfName,outname,MRTpath,UL="",LR="",resample_type="NEAREST_NEIGHBOR",proj_type="UTM",
                    bands_subset="",proj_params="0 0 0 0 0 0 0 0 0 0 0 0",datum="WGS84",utm_zone=NA,pixel_size=1000){

  	filename = file(fname, open="wt")
 	write(paste("INPUT_FILENAME = ", getwd(), "/",hdfName, sep=""), filename) 
# 	browser()
 	if (bands_subset != "") {
    		write(paste("SPECTRAL_SUBSET = ( ",bands_subset," )",sep=""),filename,append=TRUE)
  	}
  	if (UL[1] != "" & LR[1] != "") {
    		write("SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG", filename, append=TRUE)
    		write(paste("SPATIAL_SUBSET_UL_CORNER = ( ", as.character(UL[1])," ",as.character(UL[2])," )",sep=""), filename, append=TRUE)
    		write(paste("SPATIAL_SUBSET_LR_CORNER = ( ", as.character(LR[1])," ",as.character(LR[2])," )",sep=""), filename, append=TRUE)
  	}
  	write(paste("OUTPUT_FILENAME = ", outname, sep=""), filename, append=TRUE)
  	write(paste("RESAMPLING_TYPE = ",resample_type,sep=""), filename, append=TRUE)
  	write(paste("OUTPUT_PROJECTION_TYPE = ",proj_type,sep=""), filename, append=TRUE)
  	write(paste("OUTPUT_PROJECTION_PARAMETERS = ( ",proj_params," )",sep=""), filename, append=TRUE, ncolumns=5)
  	write(paste("DATUM = ",datum,sep=""), filename, append=TRUE)
  	if (proj_type == "UTM") write(paste("UTM_ZONE = ",utm_zone,sep=""), filename, append=TRUE)
  		write(paste("OUTPUT_PIXEL_SIZE = ",as.character(pixel_size),sep=""), filename, append=TRUE)
  	close(filename)
  	e <- system(paste(MRTpath, "/resample -p ",getwd(),"/",fname, sep=""))
  	e
}

setwd(WDIR)
# read in the hdf file names
WDIR2 <- dir(pattern="hdf")
WOUT2 <- paste(WDIR,"B",sep="\\")
# this is needed so R can find the MRT data dir, check the data dir on your system
Sys.setenv(MRT_DATA_DIR="c:/MODIS/data")

# run loop over files
for(i in 1:length(WDIR2)){
	#i=1
  # setting the right coordinates for two different sets
  # you might not need this
  if (grepl(WDIR2[i],"v12")==T) {
    UL_in <- c(-35.075,147.075)
    LR_in <- c(-36.675,148.925)
  } else {
    UL_in <- c(-25.575,145.825)
    LR_in <- c(-28.075,148.625)
  }
  
  # specification of the path to MRT is important
	MRTproj(fname="tmp.prm",WDIR2[i],
	        paste(WOUT2,paste(gsub(".hdf","", WDIR2[i]), 
	        ".tif", sep = ""), sep = "\\"),"C:/MODIS/bin",
	 UL=UL_in, LR=LR_in, resample_type="NEAREST_NEIGHBOR", proj_type="UTM",
	 utm_zone = 55,
	 # choose which bands to extract
	 bands_subset="1 0 0 0 0 0 1"
	, proj_params=" 0.0 0.0 0.0 0.0 0.0 0.0	0.0 0.0 0.0	0.0 0.0 0.0	0.0 0.0 0.0", 
	datum="WGS84", pixel_size=250)
}
	

