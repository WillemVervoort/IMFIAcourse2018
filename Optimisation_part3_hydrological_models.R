## ----setup, echo=F, warning=F, message=F---------------------------------
# root dir
knitr::opts_knit$set(root.dir = "C:/Users/rver4657/owncloud/Uruguay/coursematerial")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)
require(knitr)

## ----hydromad------------------------------------------------------------
require(hydromad)

## ----loadCotter, fig.cap = "plot of the data in the Cotter data set"-----
data(Cotter)
# show first 6 rows
head(Cotter)
# Make a plot of the data
xyplot(Cotter)

## ----GR4J_graphic, echo=F------------------------------------------------
include_graphics("../Data/GR4JStructure.png")

## ----simplGR4JDef--------------------------------------------------------
# specify a model structure (GR4J) with some arbritrary parameters
CMod <- hydromad(Cotter[1:1000,], sma="gr4j", routing="gr4jrouting",etmult=0.15,
                 x1 = 665, x2 = 10, x3 = 90, x4 = 3.8)

## ----plotGR4J, fig.cap="Plot of GR4J with hypothetical parameters"-------
xyplot(predict(CMod, return_state = TRUE, 
               return_components = TRUE),
       strip = FALSE, strip.left = TRUE)

## ----printPar------------------------------------------------------------
print(CMod)

## ----h_options, eval = FALSE---------------------------------------------
## hydromad.options()

## ----printCoef-----------------------------------------------------------
coefficients(CMod)

## ------------------------------------------------------------------------
# split the data to use for calibration 
Data_Cal<- window(Cotter, start = "1970-01-01",end = "1975-12-31")

## ----define_par_range----------------------------------------------------
CMod <- hydromad(Data_Cal, 
                 sma="gr4j", routing="gr4jrouting",
  etmult=c(0.05,0.5),x1 = c(1000,3000), x2 = c(-3,20), 
  x3 =c(5,500), x4 = c(0.5,10))

## ----fitByOptim----------------------------------------------------------
CotterFit <- fitByOptim(CMod,
                objective=~hmadstat("r.squared")(Q,X),
                        samples=1000,method="PORT")

## ----fit_summary---------------------------------------------------------
summary(CotterFit)

## ----coef_hydromad-------------------------------------------------------
coef(CotterFit)

## ----plotFit, fig.cap="Predicted and observed flow for Cotter for the calibration period"----
# plot observed vs modelled with the rainfall (Figure 5)
xyplot(CotterFit, with.P=TRUE, xlim=as.Date(c("1970-01-01", "1975-01-01")))

