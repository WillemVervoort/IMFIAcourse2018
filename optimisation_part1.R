## ----setup, echo=F, warning=F, message=F---------------------------------
# root dir
knitr::opts_knit$set(root.dir = "C:/Users/rver4657/owncloud/Uruguay/coursematerial")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)

## ----test_data-----------------------------------------------------------
require(tidyverse)
set.seed(20) # set seed to get same data every time

# random data x data linear regression
x <- rnorm(100)
# A function y 
y_fun <- function(x,b0, b1) 
  {
  y_out = b0 + b1*x
  # add random error
  y_out <- y_out + rnorm(length(x),0,0.5)
  
  return(y_out)
  }

## ----pars----------------------------------------------------------------
# exact pars
b0 <- 1
b1 <- 1.5

## ----show_data-----------------------------------------------------------
test_data <- data_frame(x = x, #  x data in a column called x
                        y = y_fun(x, b0,b1)) # y data from y_fun

# make a plot
test_data %>%
  ggplot(aes(x,y)) + geom_point(colour="red", size=2)

## ----lm_regression-------------------------------------------------------
mod <- lm(y~x,test_data)
summary(mod)

## ----MonteCarlo----------------------------------------------------------
# uniform samples between min and maximum values
# range of parameters
b0_range <- runif(200,0,2)
b1_range <- runif(200,0.5,3)

# define a storage data frame for the results of the objective functions
Result <- data.frame(bias = rep(0,length(b0_range)),
                     RMSE = rep(0,length(b0_range)))

# run a loop of 200 (for the 200 samples)
for (i in 1:length(b0_range)) {
  # calculate the y values
  y_pred <- y_fun(b0_range[i], b1_range[i],test_data$x)
  # Objective functions
  # calculare the bias between observed and predicted
  Result$bias[i] <- mean(y_pred - test_data$y)
  # calculate the rmse for the model
  Result$RMSE[i] <- sqrt(mean((y_pred - test_data$y)^2))
}

# plot results
Result %>%
  mutate(b0 = b0_range, b1 = b1_range) %>%
  gather(key = performance, value = value_perf,
         bias:RMSE) %>%
    gather(key = "parameter", value = "value_par", b0,b1) %>%
  ggplot(aes(value_par,value_perf, colour=parameter)) +
    geom_point() + facet_wrap(~performance, scales = "free") +
    xlab("Parameter value") + ylab("Performance value")


## ----obj_fun-------------------------------------------------------------
# define the sum squared of error as optimisation goal
obj_fun <- function(par,x,data) {
  # par is a vector of parameters b0 and b1
  # x is a vector of input x data
  # data is the observed data to fit
  resid <- data-y_fun(par[1],par[2],x)
  # penalised least squares
  SSE <- sum((resid)^2)
  return(SSE)
}


## ----optimise------------------------------------------------------------
# define some initial guesses for b0 and b1
par_in <- c(0.9,2)

fit <- optim(par_in, obj_fun, x = test_data$x, 
             data = test_data$y)

# inspect what is in fit
str(fit)

# look at the resulting objective function (SSE)
fit$value

# and at the resulting parameters
fit$par

# compare this to
b0; b1

## ----obj_fun_penalty-----------------------------------------------------
# define the sum squared of error as optimisation goal
obj_fun <- function(par,x,data, penalty) {
  # par is a vector of parameters b0 and b1
  # x is a vector of input x data
  # data is the observed data to fit
  # penalty is a penalty applied to the SSE
  resid <- data-y_fun(par[1],par[2],x)
  # penalised least squares
  SSE <- penalty*sum((resid)^2)
  return(SSE)
}


## ----optimise_pen, eval = T, echo = F------------------------------------
# define some initial guesses for b0 and b1
par_in <- c(0.9,2)

fit <- optim(par_in, obj_fun, x = test_data$x, 
             data = test_data$y, penalty = 25)

# inspect what is in fit
str(fit)

# look at the resulting objective function (SSE)
fit$value

# and at the resulting parameters
fit$par

