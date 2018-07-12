## ----setup, echo=F, warning=F, message=F---------------------------------
# root dir
knitr::opts_knit$set(root.dir = "C:/Users/rver4657/owncloud/Uruguay/coursematerial")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)
require(knitr)

## ----CreateModel, message=F, warning=F-----------------------------------
require(hydromad)
# Load the data
data(Cotter)
# select a calibration period
Data_Cal<- window(Cotter, start = "1970-01-01",end = "1975-12-31")
# Fit a model
CMod <- hydromad(Data_Cal, sma="gr4j",
                 routing="gr4jrouting",
                 etmult=c(0.05,0.5),
                 x1 = c(100,1500), x2 = c(-30,20),
                 x3 =c(5,500), x4 = c(0.5,10))
# use fitByOptim
CotterFit <- fitByOptim(CMod,
  objective=~hmadstat("r.squared")(Q,X),
  samples=1000,method="PORT")
summary(CotterFit)

## ----newdata-------------------------------------------------------------
# for validation
Data_Val <- window(Cotter, start = "1976-01-01",end = "1980-12-31")
# And a full dataset (both validation and calibration)
Data_All <- window(Cotter, start = "1970-01-01", end = "1980-12-31")

## ------------------------------------------------------------------------
Cotter_val <- update(CotterFit, newdata = Data_Val)
Cotter_all <- update(CotterFit, newdata = Data_All)

# create a runList
allMods <- runlist(calibration=CotterFit, Cotter_val, Cotter_all)


## ------------------------------------------------------------------------
# gives a statistical overview of the fits
round(summary(allMods),2)

## ------------------------------------------------------------------------
Performance <- data_frame(bias = rep(0,6),
                          r.squared = rep(0,6),
                          r.sq.sqrt = rep(0,6),
                          r.sq.log = rep(0,6))

## ------------------------------------------------------------------------
Validation_pred <- 
  data_frame(observed = Data_Cal$Q, 
             predicted = rep(0,nrow(Data_Cal)))

## ------------------------------------------------------------------------
# define begin and end dates of leave-one-out
pred_window <- cbind(c("1970-01-01","1971-01-01",
                       "1972-01-01","1973-01-01",
                       "1974-01-01"),
                     c("1971-01-01","1972-01-01",
                       "1973-01-01","1974-01-01",
                       "1975-01-01"))
 
# loop over years
for (i in 1:nrow(pred_window)) {
  # define the calibration and prediction data
  Data_pred <- window(Data_Cal,
                      start = pred_window[i,1],
                      end = pred_window[i,2])
  # remove Data_pred rows from calibration period
  "%w/o%" <- function(x, y) x[!x %in% y]
  Data_Cal2 <- Data_Cal[index(Data_Cal) 
                        %w/o% index(Data_pred),]
  # update CMod
  CMod <- update(CMod, newdata = Data_Cal2)
  # fit
  FitModel <- fitByOptim(CMod, 
            objective =hmadstat("r.squared"),
            samples=1000,method="PORT")
  # extract the performance and store
  Performance[i,] <-
    as.numeric(summary(FitModel)[7:10])
  # make prediction
  pred_mod <- update(FitModel, newdata = Data_pred)
  predicted <- predict(pred_mod)
  # extract fitted values and store
  # set start value for storage
  if (i == 1) first <- 0
  Validation_pred[(first+1):(length(predicted)+first),2] <-
                    predicted
  
  first <- length(predicted)
}

## ------------------------------------------------------------------------
Overall_perf <- apply(Performance,2,mean)
Overall_perf

# validation
(bias <- hmadstat("rel.bias")(Q = Validation_pred$observed,
  X = Validation_pred$predicted))
(NSE <- hmadstat("r.squared")(Q = Validation_pred$observed,
  X = Validation_pred$predicted))
(r.sq.sqrt <- hmadstat("r.sq.sqrt")(Q = Validation_pred$observed,
  X = Validation_pred$predicted))
(r.sq.log <- hmadstat("r.sq.log")(Q = Validation_pred$observed,
  X = Validation_pred$predicted))

## ----refit_log-----------------------------------------------------------
CMod <- update(CMod, newdata=Data_Cal)
CotterFit_log <- fitByOptim(CMod,
                   objective=hmadstat("r.sq.log"),
                        samples=1000,method="PORT")
Cotter_val_log <- update(CotterFit_log, newdata = Data_Val)

allMods <- runlist(calib.rsq = CotterFit, Cotter_val, 
                   calib_log = CotterFit_log,
                   Cotter_val_log)

round(summary(allMods),2)

## ----FDC_function, fig.width=4.5, fig.heigt=4.5, fig.cap="Example flow duration curve Cotter"----
# Maybe first write the FDC code as a function
FDC_gen <- function(DATA, plot.it=T) {
#browser()
  n = length(DATA)
  sort.flow= sort(as.numeric(DATA), decreasing = TRUE,
                na.last=FALSE)
  rank.flow <- 1:n
  Prob <- rank.flow/(n + 1)
  plot_df <- data_frame(Flow = sort.flow, Prob = Prob*100)
  # plotting
  if (plot.it ==T) {
  p <-   plot_df %>%
      ggplot(aes(Prob,Flow)) + geom_line() +
             scale_y_log10()+ 
             ggtitle("daily flow duration curve") +
      xlab("Probability") + ylab("log(Flow mm/day)")
  return(p)
  } else return(plot_df)
}

# test
FDC_gen(Cotter_val$fitted.values)
FDC_fitted <- FDC_gen(Cotter_val$fitted.values, plot.it=F)
FDC_fitted$type <- "r.squared"

## ----FDC_plot2, fig.cap="Flow duration curves for the validation, predicted with log objective function, predicted with NSE objective function and observed in the Cotter"----
# Create a data_frame with observed values
plotFDC_df <- FDC_gen(Cotter_val$data$Q, plot.it=F)
plotFDC_df$type <- "observed"
plotFDC_df <- rbind(plotFDC_df, FDC_fitted)

FDC_pred_log <- FDC_gen(Cotter_val_log$fitted.values, plot.it=F)
FDC_pred_log$type <- "r.sq.log"
plotFDC_df <- rbind(plotFDC_df, FDC_pred_log)


plotFDC_df %>%
  ggplot(aes(Prob,Flow, colour=type, linetype=type)) +
    geom_line(size=1.5) + scale_y_log10() +
    ggtitle("daily flow duration curve") +
    xlab("Probability") + ylab("log10(Flow mm/day)")


## ----include_standard_objective------------------------------------------
hydromad.options()$objective
#Using this to calibrate gives:

CotterFit_st <- fitByOptim(CMod,
                           samples=1000,method="PORT")
Cotter_val_st <- update(CotterFit_st, newdata = Data_Val)

allMods <- runlist(calib_rsq = CotterFit, Cotter_val, 
                   calib_log = CotterFit_log,  Cotter_val_log, 
                   calib_st = CotterFit_st, Cotter_val_st)

round(summary(allMods),2)

## ----FDC_plot3,  fig.cap="Flow duration curves for the validation, predicted with hydromad standard, log objective function, and NSE objective function and observed in the Cotter"----
FDC_pred_st <- FDC_gen(Cotter_val_st$fitted.values, plot.it=F)
FDC_pred_st$type <- "standard"
plotFDC_df <- rbind(plotFDC_df, FDC_pred_st)


plotFDC_df %>%
  ggplot(aes(Prob,Flow, colour=type, linetype=type)) +
    geom_line() + scale_y_log10() +
    ggtitle("daily flow duration curve") +
    xlab("Probability") + ylab("log10(Flow mm/day)")


## ---- Viney_Obj----------------------------------------------------------
# use Viney's (includes Bias), see http://hydromad.catchment.org/#hydromad.stats
hydromad.stats("viney" = function(Q, X, ...) {
    hmadstat("r.squared")(Q, X, ...) -
    5*(abs(log(1+hmadstat("rel.bias")(Q,X)))^2.5)})
# fit again
CotterFit_Vi <- fitByOptim(CMod,objective=~hmadstat("viney")(Q,X),
                        samples=1000,method="PORT")

Cotter_val_Vi <- update(CotterFit_Vi, newdata = Data_Val)

allMods <- runlist(calib_rsq=CotterFit, Cotter_val, 
                   calib.log=CotterFit_log ,  Cotter_val_log, 
                   calib_st = CotterFit_st, Cotter_val_st, 
                   calib_Vi = CotterFit_Vi,
                   Cotter_val_Vi)

round(summary(allMods),2)

## ----FDC_plot4, fig.cap="Flow duration curves for the validation, predicted with hydromad standard, log objective function, and NSE objective function and observed in the Cotter"----
FDC_pred_Vi <- FDC_gen(Cotter_val_Vi$fitted.values, plot.it=F)
FDC_pred_Vi$type <- "Viney's"
plotFDC_df <- rbind(plotFDC_df, FDC_pred_Vi)

# and plot
plotFDC_df %>%
  ggplot(aes(Prob,Flow, colour=type, linetype=type)) +
    geom_line() + scale_y_log10() +
    ggtitle("daily flow duration curve") +
    xlab("Probability") + ylab("log10(Flow mm/day)")


## ----plot_all, width = 7, height = 10, fig.cap="Plot of all the different fits and data for Cotter on a log scale"----
xyplot(allMods, scales=list(y=list(log=TRUE)))

