# reformat Paso Roldan data to hydromad zoo format

# packages
require(tidyverse, quietly = T)
require(lubridate)
require(hydromad, quietly = T)

# setwd()
setwd("C:/Users/rver4657/ownCloud/Uruguay/CourseMaterial")

# read in the original data
PR <- read_csv("data/DIARIOS HQ 117_0 Paso Roldán_sep.csv")
# gives warnings, can ignore all in indice_h
PR

# create the dates and select colum
PR_dato <- PR %>% 
  mutate(Date = dmy(fecha)) %>%
  select(Date,`datoq_m3/s`) %>%
  # 1150 km2 conversion to mm
  mutate(datoq_mm = `datoq_m3/s`*86.4/1150)


PR_dato %>%
  ggplot(aes(Date,`datoq_mm`)) + geom_line() +
  xlab("Date") + ylab("Flow in mm")

# missing data analysis
NA_length <- sum(ifelse(PR_dato$`datoq_m3/s`==NA,1,0),na.rm=T)
NA_length # filled data?

# zoom in to to 1998 - 2011 (long enough)
PR_dato %>%
  ggplot(aes(Date,`datoq_m3/s`)) + geom_line() +
  xlab("Date") + ylab("Flow in m3/s") +
  xlim(c(ymd("1998-01-01"),ymd("2011-12-31")))

# That should do for calibration and validation
# Create a zoo and subset
PR_z <- zoo(PR_dato$datoq_mm, order.by = PR_dato$Date,
            frequency = 1)
plot(PR_z)

# Read in the rainfall data
# try both Minas and Minas Campaners
Minas <- read_csv("data/Minas_1981_2017.csv")
MinasC <- read_csv("data/Minas_Campanero_1981_2017.csv")

colnames(Minas)[1] <- "Estacion"
colnames(MinasC)[1] <- "Estacion"


Minas_s <- Minas %>%
  mutate(Date = dmy(Fecha)) %>%
  filter(Date >= ymd("1998-01-01") & 
           Date <= ymd("2011-12-31"))

Minas_s %>%
  ggplot(aes(Date,as.numeric(`R3[mm]`))) + geom_line()

MinasC_s <- MinasC %>%
  mutate(Date = dmy(Fecha)) %>%
  filter(Date >= ymd("1998-01-01") & 
           Date <= ymd("2011-12-31"))

MinasC_s %>%
  ggplot(aes(Date,as.numeric(`R3[mm]`))) + geom_line()


# plot both
Both <- rbind(Minas_s,MinasC_s)
Both %>%
  ggplot(aes(Date,as.numeric(`R3[mm]`),colour=Estacion)) +
  geom_line()

#So need to fill one with the other
Minas_T <- Minas_s
Minas_T$M_R3 <- as.numeric(unlist(MinasC_s["R3[mm]"]))
Minas_T <- Minas_T %>%
  replace_na(list(`R3[mm]`=-9999,M_R3=-9999)) %>%
  mutate(missing = ifelse(`R3[mm]`==-9999,0,1),
         missing_M = ifelse(M_R3==-9999,0,1)) %>%
  mutate(Rain=ifelse((missing + missing_M)<2,
                     (`R3[mm]`*missing+ M_R3*missing_M),
                     (`R3[mm]`*missing+M_R3*missing_M)/2))
  
Minas_T %>%
  ggplot(aes(Date,Rain)) +
  geom_line()

Minas_z <- zoo(Minas_T$Rain,order.by = Minas_T$Date,
               frequency = 1)

# read in the maxT data from Las Brujas
# read in the Las Brujas temperature file
# read in the weather data:
WD <- read_csv("data/INIALasBrujas_1983-2016.csv")

# There are two different dates: 31-12-2016 and 1/1/1983
# Need to convert to standard dates
Dates <- ifelse(grepl("-",WD$Fecha)==TRUE,
                as.character(as.Date(WD$Fecha,"%d-%m-%Y")),
                as.character(as.Date(WD$Fecha,"%d/%m/%Y")))


# zoo the data and replace missing data with -99.0
# maxT
maxT_z <- zoo(WD[,3], order.by = ymd(Dates))
maxT_z[is.na(maxT_z)] <- -99.0

# merge different data files
PasoRoldan <- merge(Minas_z,PR_z,maxT_z)

head(PasoRoldan)
colnames(PasoRoldan) <- c("P","Q","E")

PasoRoldan_sub <- window(PasoRoldan,start="1998-01-01",
                         end = "2011-12-31")
plot(PasoRoldan_sub)

PasoRoldan <- PasoRoldan_sub


# write out to a RDS file
save(PasoRoldan,file="data/PasoRoldan.Rdata")

