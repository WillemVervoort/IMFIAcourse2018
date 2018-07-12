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
source("scripts/GR4J@distr.R")

## ------------------------------------------------------------------------
load("data/SL_distdata.RDATA")

## ----define_data_window--------------------------------------------------
# Use data for 2000 - 2005
Data_in <- window(SL_distdata,
                       start = "2000-01-01",
                       end ="2005-12-31")

## ----sample_pars---------------------------------------------------------
set.seed(20)
# this is for 3 subbasins
for (i in 1:3) {
  x1 <- runif(1000,50,500)
  x2 <- runif(1000,-20,20)
  x3 <- runif(1000,10,200)
  x4 <- runif(1000,0.5,5)
  if (i == 1) {
    parameterSpace <- data_frame(x1,x2,x3,x4)
  } else {
    temp <- data_frame(x1,x2,x3,x4)
    parameterSpace <- cbind(parameterSpace,temp)
  }
}
# for three subbasins
for (i in 1:2) {
  x <- runif(1000,0,0.5)
  k <- runif(1000,1,100)
  routing <- data_frame(k,x)
  parameterSpace <- cbind(parameterSpace,routing)
}

str(parameterSpace)

## ----running_model-------------------------------------------------------
# Storage for the objective functions
Store <- list()
# run a loop
for (i in 1:nrow(parameterSpace)) {
  pars_to_input <- parameterSpace[i,]
  # use the rewrite_pars() function
  input_pars <- rewrite_pars(All_par,All_reach_par,
                             pars_to_input)

  SLrun = GR4JSubBasins.run(sb=3,
                          order = c(1,2,3),
                          Data = Data_in,
                          spar = input_pars$sub,
                          rpar = input_pars$reach,
                          sbnames = c("PasoTroncos",
                                      "FrayMarcos",
                                      "PasoPache"))
  
  # calculate stats using the observed data
  # use plot = F on the plot_results function
  results <- plot_results(SLrun,Data_in, plot=F)
  # calculate stats
  stats <- stats_fun(results, decimal=2)
  # keep track of the run count
  stats$run <- i
  # put into Store
  Store[[i]] <- stats
}

Store_out <- do.call(rbind,Store)

Store_out


## ----performance_dist----------------------------------------------------
# make a hist
Store_out %>%
  # gather all the performance values
  gather(key = "performance", value = "value",
         r.squared:r.sq.log) %>%
  # make a histogram for each
  ggplot(aes(value, fill = Subbasin)) +
  geom_histogram(alpha=0.5) + facet_wrap(~performance) +
  xlim(c(-2,1))
# wrap by performance measure


## ----arrange-------------------------------------------------------------
Store_out_sort <- Store_out %>%
  arrange(Subbasin)
# note the order, as the Subbasins are ordered:
# FrayMarcos, PasoPache, PasoTroncos
parameterSpace_stacked <- rbind(parameterSpace[,5:8],
                                parameterSpace[,9:12],
                                parameterSpace[,1:4])

# now combine
Store_out_comb <- cbind(Store_out_sort,parameterSpace_stacked)

## ------------------------------------------------------------------------
Store_out_comb %>%
  gather(key="parameter", value="value", x1:x4) %>%
  ggplot(aes(value,r.squared,colour=Subbasin)) +
  geom_point( alpha=0.5) +  facet_wrap(~parameter,
                                       scales="free")

## ----Threshold-----------------------------------------------------------
GLUE_th <- 0.5
Store_out_g <- Store_out %>%
  filter(r.squared > GLUE_th & r.sq.sqrt > GLUE_th &
           r.sq.log > GLUE_th)


## ----match_pars----------------------------------------------------------
Glue_Store <- cbind(Store_out_g,
                    parameterSpace[Store_out_g$run,])



## ----rerun_models--------------------------------------------------------
# Storage for the model results
FrayMarcos <- NULL
PasoPache <- NULL
PasoTroncos <- NULL
Results <- list("PasoTroncos"=PasoTroncos,
                "FrayMarcos" =FrayMarcos,
                "PasoPache"=PasoPache)

# run a loop
for (i in 1:nrow(Glue_Store)) {
 # i <- 1
  pars_to_input <- Glue_Store[i,7:ncol(Glue_Store)]
  # fix names of parameters
  names(pars_to_input) <- c(rep(c("x1","x2","x3","x4"),3),
                            rep(c("x","k"),2))
  # use the rewrite_pars() function
  input_pars <- rewrite_pars(All_par,All_reach_par,
                             pars_to_input)

  SLrun = GR4JSubBasins.run(sb=3,
                          order = c(1,2,3),
                          Data = Data_in,
                          spar = input_pars$sub,
                          rpar = input_pars$reach,
                          sbnames = c("PasoTroncos",
                                      "FrayMarcos",
                                      "PasoPache"))

  # Select only the station that is well modelled from the output
if (length(Results[[Glue_Store$Subbasin[i]]]) ==0) {
      Results[[Glue_Store$Subbasin[i]]] <-
    SLrun[,colnames(SLrun) %in% Glue_Store$Subbasin[i]]
  } else {
      Results[[Glue_Store$Subbasin[i]]] <-
    merge(Results[[Glue_Store$Subbasin[i]]],
          SLrun[,colnames(SLrun) %in% Glue_Store$Subbasin[i]])
  }
}


## ----plotGluePT----------------------------------------------------------
# Paso Los Troncos
AvgResults <- apply(Results[[1]],1,mean)
sdResults <- apply(Results[[1]],1,sd)
plot_df <- as.tibble(cbind(AvgResults,
                           (AvgResults - 2*sdResults),
                           (AvgResults + 2*sdResults)))
names(plot_df) <- c("mean","neg_95ci", "pos_95ci")
plot_df <- plot_df %>%
  mutate(fecha = ymd(time(Data_in))) 
plot_df %>%
  gather(key="Glue_results", value="value", mean:pos_95ci) %>%
  ggplot(aes(fecha,value, colour=Glue_results,
             linetype=Glue_results)) + geom_line(size=1.5) +
  ggtitle("PasoLosTroncos") + scale_y_log10()

plot_df %>%
  gather(key="Glue_results", value="value", mean:pos_95ci) %>%
  ggplot(aes(fecha,value, colour=Glue_results,
             linetype=Glue_results)) + geom_line(size=1.5) +
  ggtitle("PasoLosTroncos") + 
  xlim(c(ymd("2002-01-01"),ymd("2003-01-01")))


## ----plotGlueFM----------------------------------------------------------
# Fray Marcos
AvgResults <- apply(Results[[2]],1,mean)
sdResults <- apply(Results[[2]],1,sd)
plot_df <- as.tibble(cbind(AvgResults,
                           (AvgResults - 2*sdResults),
                           (AvgResults + 2*sdResults)))
names(plot_df) <- c("mean","neg_95ci", "pos_95ci")
plot_df <- plot_df %>%
  mutate(fecha = ymd(time(Data_in))) 
plot_df %>%
  gather(key="Glue_results", value="value", mean:pos_95ci) %>%
  ggplot(aes(fecha,value, colour=Glue_results,
             linetype=Glue_results)) + geom_line(size=1.5) +
  ggtitle("Fray Marcos") + scale_y_log10()



## ----plotGluePP----------------------------------------------------------
# Paso Pache
AvgResults <- apply(Results[[1]],1,mean)
sdResults <- apply(Results[[1]],1,sd)
plot_df <- as.tibble(cbind(AvgResults,
                           (AvgResults - 2*sdResults),
                           (AvgResults + 2*sdResults)))
names(plot_df) <- c("mean","neg_95ci", "pos_95ci")
plot_df <- plot_df %>%
  mutate(fecha = ymd(time(Data_in))) 
plot_df %>%
  gather(key="Glue_results", value="value", mean:pos_95ci) %>%
  ggplot(aes(fecha,value, colour=Glue_results,
             linetype=Glue_results)) + geom_line(size=1.5) +
  ggtitle("Paso Pache") + scale_y_log10()



