## ----setup, echo=F, warning=F, message=F---------------------------------
# root dir
knitr::opts_knit$set(root.dir = "C:/Users/rver4657/owncloud/Uruguay/coursematerial")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)
require(hydromad)
require(knitr)

## ---- eval = FALSE-------------------------------------------------------
## require(tidyverse)
## require(lubridate)
## require(hydromad)

## ------------------------------------------------------------------------
source("scripts/GR4J@distrv2.R")

## ------------------------------------------------------------------------
load("data/SL_distdata.RDATA")

## ----define_data_window--------------------------------------------------
# Use data for 2000 - 2005
Data_in <- window(SL_distdata,
                       start = "2000-01-01",
                       end ="2005-12-31")

## ----Define_initial_guess------------------------------------------------
# initial guesses, using the SubX.par in All_par. and
# reachX.par in All_reach_par, where X is a number
x <- c(All_par$Sub1.par[2:5])

## ----Define_all_pars-----------------------------------------------------
# all the parameters of the model
par_in <- c(All_par$Sub1.par[1:5], All_par$Sub2.par[1:5],
  All_par$Sub3.par[1:5], All_reach_par$reach1.par,
  All_reach_par$reach2.par)

## ----define_Fit_theseS1--------------------------------------------------
# define which parameters to calibrate
# We want to
Fit_these <- c(2:5) 

## ----define_rest_model---------------------------------------------------
# Use data for 2000 - 2005
# created earlier as Data_in
# define the model_input (optional, as these are the defaults)
model_input_in <- list(sb = 3, order = c(1,2,3),
                        sbnames = c("PasoTroncos",
                        "FrayMarcos",
                        "PasoPache"))

## ----fit_model_optim-----------------------------------------------------
Fit_SL <- optim(par = x, # values of the parameters to fit
    DistGR4J_objfun, # function to fit
    calibrate_on = Fit_these, # which positions of parameters
    parM = par_in, # all parameters
    Data = Data_in, # The input data (Q,P,E)
    model_input = model_input_in, # (opt) rest model input
      objective = "r.squared", # objective function(optional)
    weights = c(1,0,0)) # weights

Fit_SL$par
Fit_SL$value

## ----_rewrite_parsS2-----------------------------------------------------
# insert fitted values into parameters
# using rewrite_pars() Note the change in definition
# this creates a list of reach and sub parameters
new_pars <- rewrite_pars(All_par,changed_par = Fit_SL$par,
                         sb = 1, which = 1)

## ----Define_initial_guessS2----------------------------------------------
# only subbasin 2 and reach 1
x <- c(All_par$Sub2.par[2:5], All_reach_par$reach1.par)

## ----define_Fit_theseS2--------------------------------------------------
# define which parameters to calibrate
# We want to
Fit_these <- c(7:10,16,17) 

## ----fit_model_optimS2---------------------------------------------------
Fit_SL <- optim(par = x, # values of the parameters to fit
    DistGR4J_objfun, # function to fit
    calibrate_on = Fit_these, # which positions of parameters
    parM = par_in, # all parameters
    Data = Data_in, # The input data (Q,P,E)
    model_input = model_input_in, # (opt) rest model input
      objective = "r.squared", # objective function(optional)
    weights = c(0,1,0)) # weights 

Fit_SL$par
Fit_SL$value

## ----_rewrite_parsS3-----------------------------------------------------
# insert fitted values into parameters
# using rewrite_pars()
new_pars <- rewrite_pars(All_par,All_reach_par,
                         changed_par = Fit_SL$par,
                         sb = 2, which = 2)

## ----Define_initial_guessS3----------------------------------------------
# only subbasin 2 and reach 1
x <- c(All_par$Sub3.par[2:5], All_reach_par$reach2.par)

## ----define_Fit_theseS3--------------------------------------------------
# define which parameters to calibrate
# We want to
Fit_these <- c(12:15,18,19) 

## ----fit_model_optimS3---------------------------------------------------
Fit_SL <- optim(par = x, # values of the parameters to fit
    DistGR4J_objfun, # function to fit
    calibrate_on = Fit_these, # which positions of parameters
    parM = par_in, # all parameters
    Data = Data_in, # The input data (Q,P,E)
    model_input = model_input_in, # (opt) rest model input
      objective = "r.squared", # objective function(optional)
    weights = c(0,0,1)) # weights 

Fit_SL$par
Fit_SL$value

## ----_rewrite_pars-------------------------------------------------------
# insert fitted values into parameters
# using rewrite_pars()
# this creates a list of reach and sub parameters
new_pars <- rewrite_pars(All_par,All_reach_par,
                         changed_par = Fit_SL$par,
                         sb = 1, which = 3)

## ----predict_model-------------------------------------------------------
Fitted_SL <- GR4JSubBasins.run(sb=3,
                          order = c(1,2,3),
                          Data = Data_in,
                          spar = new_pars$sub,
                          rpar = new_pars$reach,
                          sbnames = c("PasoTroncos",
                                      "FrayMarcos",
                                      "PasoPache"))

xyplot(Fitted_SL)

## ----plot_fitted_obs-----------------------------------------------------
# plot using `plot_results()`
results <- plot_results(Fitted_SL,Data_in)
# this creates a data frame with observed and fitted Q

## ----calculate_stats-----------------------------------------------------
# using stats_fun on results
stats_fun(results, decimal=2)


