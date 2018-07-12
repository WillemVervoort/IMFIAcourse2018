# GLUE and DREAM demo
require(hydromad)
require(dream)

data(Cotter)
# select a calibration period
Data_Cal<- window(Cotter, start = "1970-01-01",end = "1975-12-31")
# Fit a model
CMod <- hydromad(Data_Cal, sma="gr4j",
                 routing="gr4jrouting",
                 etmult=c(0.05,0.5),
                 x1 = c(100,1500), x2 = c(-30,20),
                 x3 =c(5,500), x4 = c(0.5,10))

# start with GLUE
CotterFitSample <- fitBySampling(CMod,samples=5000)

summary(CotterFitSample)
## plot objective function value improvement over time
windows()
xyplot(optimtrace(CotterFitSample), type = "b",
       xlab = "function evaluations", ylab = "objective fn. value")

# so what should the threshold be?

## Calculate 5 percent and 95 percent GLUE quantiles (i.e. weighted).
Cotterfitglue <- defineFeasibleSet(CotterFitSample, 
                                   threshold = 0.7,
                            glue.quantiles = c(0, 1))

xyplot(Cotterfitglue, feasible.bounds = TRUE, cut = 3,
       scales = list(y = list(log = TRUE)))

# target.coverage (similar to SUFI2 in SWAT-CUP)
Cotterfitglue2 <- defineFeasibleSet(CotterFitSample, 
                                    target.coverage = 0.9,
                              glue.quantiles = c(0.05, 0.95))

xyplot(Cotterfitglue2, feasible.bounds = TRUE, cut = 3,
       scales = list(y = list(log = TRUE)))



#Run DREAM, start with a run of 1000 to see how long this takes
# use system.
## a short run to demonstrate methods
system.time({Cotter.dream <- fitByDream(CMod, control = list(nseq = 10,ndraw=5000))})

summary(Cotter.dream)
# better result
# gives a variance covariance matrix of the parameters
symnum(cov2cor(vcov(Cotter.dream)))
# numeric
round(cov2cor(vcov(Cotter.dream)),2)
# we can see some high correlation between x2, x3 and etmult. Does this make sense?

## calculate corresponding objective function values over time.
xyplot(optimtrace(Cotter.dream),
       xlab = "function evaluations", ylab = "Objective function")

# and the results (on log scale)
xyplot(Cotter.dream, scales = list(y = list(log = TRUE)))
# and finally
plot(Cotter.dream$fit.result)

Dreamglu <- defineFeasibleSet(Cotter.dream, 
                              target.coverage = 1,
                              glue.quantiles = c(0.05, 0.95))
xyplot(Dreamglu, feasible.bounds = TRUE, cut = 3,
       scales = list(y = list(log = TRUE)))



