## ----setup, echo=F, warning=F, message=F---------------------------------
# root dir
knitr::opts_knit$set(root.dir = "C:/Users/rver4657/owncloud/Uruguay/coursematerial")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)
require(knitr)

## ----packagesInstall, eval=F---------------------------------------------
## install.packages(c("randtoolbox", "sensitivity"))

## ----packages_Data, message=F, warning=F---------------------------------
require(hydromad)
require(randtoolbox)
require(sensitivity)

# again set.seed() to make results repeatable
set.seed(20)

# Load the data
data(Cotter)
# select a shorter period
Data_Cal<- window(Cotter, start = "1970-01-01",end = "1975-12-31")

## ----model_def-----------------------------------------------------------
CMod <- hydromad(Data_Cal, sma="gr4j",
                 routing="gr4jrouting",
  etmult=c(0.05,0.5),x1 = c(100,1500), x2 = c(-30,20), 
  x3 =c(5,500), x4 = c(0.5,10))

## ----fitBysampling-------------------------------------------------------

FBS <- fitBySampling(CMod,
              objective =~hmadstat("r.squared")(Q,X),
              samples=500, sampletype = "latin.hypercube")

summary(FBS)

## ----regression----------------------------------------------------------
reg_df <- FBS$fit.result$psets
reg_df$objseq <- FBS$fit.result$objseq

# regression
lm_mod <- lm(objseq~x1 + x2 + x3 + x4 + etmult,
             data = reg_df)
summary(lm_mod)


## ----morrishelp, eval =FALSE---------------------------------------------
## ?morris

## ----Morris--------------------------------------------------------------
mm <- morris(model=evalPars,
             factor=names(getFreeParsRanges(CMod)),
             r=1000,
             design=list(type="oat",
                         levels=10, grid.jump=2),
             binf=sapply(getFreeParsRanges(CMod),min),
             bsup=sapply(getFreeParsRanges(CMod),max),
             object=CMod,
             objective=~hmadstat("r.squared")(Q,X)
)
mm

## ----SOBOL---------------------------------------------------------------
n <- 1000
X1 <- hydromad::parameterSets(getFreeParsRanges(CMod),n)
X2 <- hydromad::parameterSets(getFreeParsRanges(CMod),n)
Sob_sens <- sobol2002(model = evalPars,
               X1 = X1, X2 = X2,
               nboot = 100,
               object=CMod,
               objective=~hmadstat("r.squared")(Q,X)
               
)
Sob_sens

