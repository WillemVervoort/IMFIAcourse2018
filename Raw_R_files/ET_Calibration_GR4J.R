## ----setup, echo=F, warning=F, message=F---------------------------------
# root dir
knitr::opts_knit$set(root.dir = "C:/Users/rver4657/owncloud/Uruguay/coursematerial")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)
require(hydromad)
require(knitr)

## ----logos, echo=F-------------------------------------------------------
include_graphics("logos.png")

## ---- eval = F-----------------------------------------------------------
## require(tidyverse)
## require(lubridate)

## ----MODISts-------------------------------------------------------------
MODIS_ts <- function(MODISdir="MODIS",
                     patt=".asc"){
  
  # step 1: read in all the file names
   x1 <- list.files(path=MODISdir, pattern=patt)
  
  # step 2: rows and storage
  # each "asc" file stores all the values in time for 1 location
 # the number of rows is important as this is all the time steps
   # check the number of rows in the file
    n <- nrow(read.csv(paste(MODISdir,x1[1],sep="/"),header=F,
                       na.strings=32765))
    # use this information to:
    # Create storage for the data, Jdate is Julian date
    Store <- data.frame(Year = numeric(length=n),
                        Jdate = numeric(length=n),
                        ET = numeric(length=n),
                        Point = numeric(length = n))
    
    # Create an empty list to store the different pixels 
    # (each with a data frame called Store)
    # step 3: create a "master" storage list
    Store1 <- list()
    # Step 4: run a loop over the list of file names
    for (i in 1:length(x1)) {
      Mdata <- read.csv(paste(MODISdir,x1[i],sep="/"),header=F,
                        na=32765)
      # do some substringing
      Store[,1] <- as.numeric(substr(Mdata[1:n,8],2,5))
      Store[,2] <- as.numeric(substr(Mdata[1:n,8],6,8))
      Store[,3] <- Mdata[1:n,11]*0.1
      Store[,4] <- i
      Store1[[i]] <- Store

    }
    # step 5: converting from list back to a data.frame
    ts.data <- do.call(rbind,Store1) 
    # Now make the date from the Year and Jdate
    ts.data$Date <- as.Date(paste(ts.data$Year,ts.data$Jdate, 
                                  sep = "/"), "%Y/%j")
    
    return(ts.data)
}

# and we can run this function and check the data
SL_ET <- 
  MODIS_ts(MODISdir = "Data/Modis")
# Chk the data
head(SL_ET)


## ----plot_ts,fig.cap="Figure 1 Timeseries of ET at the first five points"----
SL_ET %>%
  filter(Point>= 10 & Point < 16) %>%
  filter(Year >= 2010 & Year < 2012) %>%
  ggplot(aes(Date,ET, colour=as.factor(Point))) +
               geom_point(size=2) +  
     xlab("8-day time series") + ylab("8-day ET in mm") +
       ylim(c(0,50))

## ----plot_hist,fig.cap="Figure 2 Histogram of MODIS ET across the different points"----
SL_ET %>%
  ggplot(aes(ET)) + geom_histogram(fill="blue") +
        xlab("MODIS ET 8 day cumulative values")

## ------------------------------------------------------------------------
SL_ET_no0 <- SL_ET %>%
  filter(ET > 0)

## ----ETmean, fig.cap="Figure 3 Average ET across the catchment"----------
SL_ET_mean <- SL_ET_no0 %>%
  group_by(Year,Jdate) %>%
  summarise(meanET = mean(ET, na.rm = T),
            sdET = sd(ET, na.rm = T))
# Check the data
SL_ET_mean

## ----dateconvert---------------------------------------------------------
SL_ET_mean <- SL_ET_mean %>%
  mutate(Date = ymd(paste(Year,"01-01",sep="-")) +
           Jdate) 
# Now, make a plot of the mean 8 daily ET 
SL_ET_mean %>%
  ggplot(aes(Date, meanET)) + 
  geom_line(colour="blue", size=1.2) +
  xlab("Time (8-daily)") + ylab("Basin average ET")

## ------------------------------------------------------------------------
SL_ET_mean %>%
  gather(key = "variable", value="value", meanET, sdET) %>%
  ggplot(aes(Date, value, colour=variable)) + 
  geom_line(size=1.2) + 
  facet_wrap(~variable) +
  xlab("Time (8-daily)") + ylab("Basin average and sd ET")


## ------------------------------------------------------------------------
require(hydromad)

## ----loaddata------------------------------------------------------------
load("data/PasoPache.Rdata")

## ----convertToZoo--------------------------------------------------------
# Converting to a zoo format to work with hydromad
SL_MODISET <- zoo(SL_ET_mean$meanET, order.by=SL_ET_mean$Date)

## ----sourceFun-----------------------------------------------------------
source("Rcode_IMFIA_course2018/leapfun.R")
source("Rcode_IMFIA_course2018/ETa.merge.R")
source("Rcode_IMFIA_course2018/plot.ET.R")
source("Rcode_IMFIA_course2018/ETfit.objectives.R")


## ----dataLoad_merge, fig.cap="Figure 5: Merged dataset for the Santa Lucia Catchment"----

# discard the data before 2000
PP_data <- window(PasoPache, start="2000-01-01")

PP_Sat <- ETa.merge(Flowdata=PP_data,ETdata=SL_MODISET)
# Make a plot
xyplot(PP_Sat)

## ----GR4Jcalibration, fig.cap="Figure 6: GR4J model fitted with local station data"----
# Data period for calibration
data_cal <- window(PP_data, start = "2000-01-01",end = "2005-12-31")

# Data for validation period
data_val <- window(PP_data, start = "2006-01-01",end = "2010-12-31")

# Define the model, important to define return_state=T
# seeting etmult = 1, as the data is potential ET
SL_mod <- hydromad(DATA=data_cal,
                   sma = "gr4j", routing = "gr4jrouting", 
                   x1 = c(100,1000), x2 = c(-10,5), 
                   x3 = c(1,300), 
                   x4 = c(0.5,10), etmult=1, 
                   return_state=TRUE)

# Using shuffled complex evolution algorithm for fitting
SL_fit<- fitBySCE(SL_mod,  
                     objective= hmadstat("r.squared"))

# Extract the coefficients and the summary
summary(SL_fit)
# plot
xyplot(SL_fit, with.P = TRUE)

## ----CalibrationEta, fig.cap="Figure 7: GR4J Model fitted with local station and Satellite ET data"----
# remake the calibration data
data_modis_cal <- window(PP_Sat, start = "2000-01-01",end = "2005-12-31")

# also make the validation data
data_modis_val <- window(PP_Sat, start = "2006-01-01",end = "2010-12-31")

# Because we have rebuilt data.cal, redefine the model
SL_mod_Modis <- hydromad(DATA=data_modis_cal,
                   sma = "gr4j", routing = "gr4jrouting", 
                   x1 = c(100,1000), x2 = c(-10,5), 
                   x3 = c(1,300), x4 = c(0.5,10), 
                   etmult=1, 
                   return_state=TRUE)


# fit both ET and Q using special objective function
SL_Fit_Modis <- fitBySCE(SL_mod_Modis,
            objective=~hmadstat("JointQandET")(Q,X,w=0.5,
            DATA=DATA,model=model,objf = hmadstat("r.squared")))

# check the model fit
summary(SL_Fit_Modis)
## 
# Plotting the results
xyplot(SL_Fit_Modis, with.P = TRUE)

## ----plotET, fig.cap="Plot showing observed vs predicted actual ET after calibration"----
plot.ET(data=data_modis_cal,SL_Fit_Modis)

## ----plot_ET_wo, , fig.cap="Plot showing observed vs predicted actual ET, w/o calibration"----
plot.ET(data=data_modis_cal,SL_fit)

## ------------------------------------------------------------------------
hmadstat("r.squared")(Q=data_modis_cal$aET,X=SL_fit$U$ET)
hmadstat("r.squared")(Q=data_modis_cal$aET,X=SL_Fit_Modis$U$ET)

## ------------------------------------------------------------------------
require(epiR)
epi.ccc(data_modis_cal$aET,SL_fit$U$ET)$rho.c
epi.ccc(data_modis_cal$aET,SL_Fit_Modis$U$ET)$rho.c



## ----ModelPerformance----------------------------------------------------
coef(SL_fit)
coef(SL_Fit_Modis)
objFunVal(SL_fit)
objFunVal(SL_Fit_Modis)

## ----ValPerformance------------------------------------------------------
# updating the model data for the validation
SL_val <- update(SL_fit, newdata = data_val)
SL_val_modis <- update(SL_Fit_Modis, newdata = data_modis_val)

# runlist
allMods <- runlist(calibration=SL_fit, validation=SL_val,
                   calibrationET=SL_Fit_Modis, 
                   validationET= SL_val_modis)

# Get the summary results
round(summary(allMods),2)

