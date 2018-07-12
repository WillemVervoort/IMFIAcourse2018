## ----setup, echo=F, warning=F, message=F---------------------------------
# root dir
knitr::opts_knit$set(root.dir = "C:/Users/rver4657/owncloud/Uruguay/coursematerial")
knitr::opts_chunk$set(echo = TRUE)
require(tidyverse)
require(lubridate)

## ---- eval = F-----------------------------------------------------------
## install.packages("bbmle")

## ------------------------------------------------------------------------
require("bbmle")

## ----random_set----------------------------------------------------------
set.seed(1001)

# 100 random samples, mean =3 sd = 2
N <- 100
x <- rnorm(N, mean = 3, sd = 2)

mean(x)
sd(x)

## ----LL_fun--------------------------------------------------------------
LL <- function(mu, sigma) {
    # define perfect normal distribution
     R = suppressWarnings(dnorm(x, mu, sigma))
     # likelihood: minus sum of logged values (one number)
     -sum(log(R))
}

## ------------------------------------------------------------------------
# use "mle()" to fit
mle(LL, start = list(mu = 1, sigma=1))

## ----test_data-----------------------------------------------------------
require(tidyverse)

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

b0 = 1
b1 = 1.5

test_data <- data_frame(x = x, #x data in a column called x
                        y = y_fun(x, b0,b1)) # y data from y_fun


## ----lm_regression-------------------------------------------------------
mod <- lm(y~x,test_data)
summary(mod)

## ----plot_with_lmline----------------------------------------------------
test_data %>%
  ggplot(aes(x,y)) + geom_point(colour="red", size=3) +
  geom_smooth(method="lm", formula = y~x)


## ----mLL-----------------------------------------------------------------
# Now using maximum likelihood:
LL <- function(beta0, beta1, mu, sigma) {
  # Find residuals from linear model
  #
  R = y_in - x_in * beta1 - beta0
  #
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  # These should be normal!!
  R = suppressWarnings(dnorm(R, mu, sigma))
  #
  # Sum the log likelihoods for all of the data points
  # This is the objective function
  -sum(log(R))
}

# There is a problem if you choose values far away from reality
# But reasonable estimates give reasonable results
y_in <- test_data$y
x_in <- test_data$x
fit <-  mle2(LL, start = list(beta0 = 0.5, beta1 = 3, 
                            mu=0, sigma=1))
fit
summary(fit)

## ----plot_LL-------------------------------------------------------------
test_data %>%
  mutate(LLfit = fit@coef[1] + fit@coef[2]*test_data$x) %>%
  ggplot(aes(x,y)) + geom_point(colour="red", size=3) +
  geom_smooth(method="lm", formula = y~x, se = F) +
  geom_line(aes(x,LLfit), col="green")


## ----fix_mu0-------------------------------------------------------------
fit0 <- mle2(LL, data=test_data, start = list(beta0 = 4, 
  beta1 = 2, sigma=1), fixed = list(mu=0))
fit0
summary(fit0)


## ----plot_LL0------------------------------------------------------------

test_data %>%
  mutate(Lmfit = mod$coefficients[1] + 
           mod$coefficients[2]*x) %>%
  mutate(LLfit = fit@coef[1] + fit@coef[2]*x) %>%
  mutate(LLfit0 = fit0@coef[1] + 
           fit0@coef[2]*x) %>%
  gather(key="method", value="values",
         Lmfit,LLfit, LLfit0) %>%
  ggplot(aes(x,y)) +
  geom_point(colour="blue", size=3) +
  geom_line(aes(x,values, col=method, 
                linetype=method, size=method)) +
              scale_size_manual(values=c(1.5,1.5,1))

## ------------------------------------------------------------------------
AIC(mod)
AIC(fit)
AIC(fit0)

