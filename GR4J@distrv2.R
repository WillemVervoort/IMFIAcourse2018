#GR4J (distributed @ 3sub-basins)+ muskingum routing (2 reaches) for Santa Lucia
# Rafael Navas and Willem Vervoort 2018
# rm(list=ls())
# Now includes utility functions to plot and rewrite parameters
# Also to calculate stats

require(hydromad)

# #load data and parameters
# #########################
# load("C:/Users/rver4657/ownCloud/Uruguay/CourseMaterial/Data/GR4J_DataAndParameters2.RDATA")
#############################################################################################################
#GR4J_DataAndParameters2.RDATA contains:												#
#																		#
#																		#
#Reach1.par, Reach2.par: list with reach parameters										#
#				k , x , length.km (k,x, muskingum parameters, x ->[0,0.5])					#
#																		#
#SubBasin1.data, SubBasin2.data, SubBasin3.data: zoo type with streamflow, areal precipitation and evapotranspiration	#
#								Q,P and ETP is not distributed							#
#																		#
#SubBasin1.par, SubBasin2.par, SubBasin3.par": list with subbasin parameters (GR4J)					#
# 						etmult,x1, x2, x3, x4, S_0, R_0, warmup, surface.km				#
#############################################################################################################
#muskingum function
####################################
muskingum <- function(q_in,k,x,dt){
	c1 = (dt-2*k*x) / (2*k*(1-x)+dt)
	c2 = (dt+2*k*x) / (2*k*(1-x)+dt)
	c3 = (2*k*(1-x)-dt) / (2*k*(1-x)+dt)
	q_out=q_in[1]
	for(i in 1:length(q_in)){
		qc1 = q_in[i]*c1
		qc2 = q_in[i-1]*c2
		qc3 = q_out[i-1]*c3
		q_out = c(q_out,qc1+qc2+qc3)
	}
	q_out[q_out<0] = 0
	return(q_out)
}
#################################### 


#GR4J model w SubBasins
####################################
GR4JSubBasins.run = function(sb=3,order = c(1,2,3),Data,
                            spar=NULL,
                            rpar=NULL,
                            sbnames = NULL,
                            return_state = F) {
# sb is the number of subbasins to simulate
# order = the order in which the basins are connected
# Data = A zoo data frame with the data for all the subcatchments
  #     using "order" with column names Q_i, P_i and E_i, where
  #     i stands for the number of the subcatchment
# spar = list with **named** parameter estimates for the subbasins
# rpar = list with **named** parameters for routing
# sbnames = vector of length sb with names of the sub catchment (optional)  
# return_state = whether the input data should be returned

# run GR4J for each sub basin
  Out_store <- list()
  #browser() 
 
 for (i in 1:sb) {
    Data_input <- Data[,(1 + (sb*(i-1))):(3 + (sb*(i-1)))]
    names(Data_input) <- c("Q","P","E")
       Out_store[[i]] = hydromad(Data_input, sma="gr4j", 
                              routing="gr4jrouting",
                              etmult=spar[[i]]$etmult,
                              x1=spar[[i]]$x1, x2=spar[[i]]$x2, 
                              x3=spar[[i]]$x3, x4=spar[[i]]$x4)
 }
  # collect the fitted values as output
  qout <- list()
  for (i in 1:length(order)) {
    qout[[i]] <- as.vector(Out_store[[order[i]]]$fitted.values)
  }
 
	#Muskingum routing 
   monitor_out <- list()
   reach_out <- list()
   # save the first monitoring point
   monitor_out[[1]] <- qout[[1]]
   # routing of the first sub catchment
	 reach_out[[1]] <- muskingum(monitor_out[[1]],rpar[[1]]$k,
	                             rpar[[1]]$x,rpar[[1]]$d_t)
	 # routing of subsequent subcatchments
	 for (i in 2:(length(order)-1)) {
	   monitor_out[[i]] <- reach_out[[i-1]] + qout[[i]]
	   reach_out[[i]] <- muskingum(monitor_out[[i]],rpar[[i]]$k,
	                               rpar[[i]]$x,rpar[[i]]$d_t)
	 }
    monitor_out[[length(order)]] <- reach_out[[length(order)-1]] + 
      qout[[length(order)]]
	#output variables (SMA results are ommited)
  if (length(sbnames) > 0) {
    names(monitor_out) <- sbnames
  }
  # create output as a zoo data.frame
  #browser()
  if (return_state == T) {
    Output <- do.call(cbind,monitor_out)
    Output <- zoo(Output, order.by=time(Data))
    Output <- merge(Data,Output)
    return(Output)
  }  else {
    Output <- do.call(cbind,monitor_out)
    Output <- zoo(Output, order.by=time(Data))
    return(Output)
  }
    
}
####################################


# #single run of SL3SubBasins.run
# ###################################
# 
# SLrun = GR4JSubBasins.run(sb=3, order = c(1,2,3),
#                          list(SubBasin1.data,SubBasin2.data,SubBasin3.data),
#                          list(SubBasin1.par,SubBasin2.par,SubBasin3.par),
#                          list(Reach1.par,Reach2.par),
#                          sbnames = c("PasoTroncos", "FrayMarcos",
#                                      "PasoPache"))

# #Model performance (without calibration)
# library(topmodel)
# 
# NSeff(SLrun$PasoPache,SubBasin1.data$PasoPache) 	#.57
# NSeff(SLrun$FrayMarcos,SubBasin2.data$FrayMarcos) 	#.71
# NSeff(SLrun$PasoTroncos,SubBasin3.data$PasoTroncos) 	#.54


# Create a function to optimise the distributed model
DistGR4J_objfun <- function(x, calibrate_on, 
                              parM, Data,
                              model_input = list(sb = 3,
                                order = c(1,2,3),
                                return_state = F,
                               sbnames = c("PasoTroncos", "FrayMarcos", "PasoPache")),
                            objective = "r.squared", 
                            weights= c(1,1,1)) {
  # x is a vector with the parameters to optimise
  # par is a vector with all the parameters for the model
  # par.sub1, par.sub2, parsub...., rpar1, rpar2,....
  # the length of par depends on the number of subbasins
  # calibrate_on, is the positions of the parameters in par related to x
  # model_input is a list of all the model input except the paramaters
  # weights is a vector of weights to apply to the subcatchments
  # in the calibration, at least one has to be > 0

  # identify the number of subbasins
  no_of_sb <- model_input$sb
#  browser()
  # redefine the parameters using x
  parM[calibrate_on] <- x
  # check if parameters valid
  named_x <- parM[grep("x",names(parM))]
  if (any(parM[grep("etmult",names(parM))] < 0.01) ||
      any(parM[grep("etmult",names(parM))] > 1) || 
      any(parM[grep("x1",names(parM))] < 1) ||
      any(parM[grep("x3",names(parM))] < 1) || 
      any(parM[grep("x4",names(parM))] < 0.5) ||
      any(parM[grep("k",names(parM))] < 0) ||
      any(named_x[names(named_x)=="x"] < 0) ||
      any(named_x[names(named_x)=="x"] > 0.5)) {
        return(99999) # return a large value 
    } else {
    # run the model with inputs
    #browser()
        model_output <- GR4JSubBasins.run(sb = model_input$sb,
                                      order = model_input$order,
                                      Data = Data,
                                      spar = list(parM[1:5], parM[6:10], 
                                                  parM[11:15]),
                                      rpar = list(parM[16:18],parM[19:21]),
                                      sbnames = model_input$sbnames)
    #browser()                 
    # compare with observed
    Obj_fun <- list()
    for(i in 1:no_of_sb) {
      if(ncol(model_output)<(3*no_of_sb)) {
          X_out <- model_output[,i]
      } else {
          X_out <-   model_output[,i+3*no_of_sb]
      }
      Q_obs <- Data[,grep("Q",colnames(Data))]
      Obj_fun[[i]] <- weights[i]*hmadstat(objective)(Q = Q_obs[,i],
                                          X = X_out)
    }
    
    
    return(-1*mean(unlist(Obj_fun)))
    
  }
}


# x_in <- c(SubBasin1.par[1:5], SubBasin2.par[1:5],
#              SubBasin3.par[1:5], Reach1.par[1:2],
#              Reach2.par[1:2])
# par_in <- c(SubBasin1.par[1:5], SubBasin2.par[1:5],
#             SubBasin3.par[1:5], Reach1.par[1:3],
#             Reach2.par[1:3])
# # define the data list
# Data_in <- list(SubBasin1.data, SubBasin2.data,
#                 SubBasin3.data)
# # define the model_input
# model_input_in <- list(sb = 3, order = c(1,2,3),
#                        Data = Data_in,
#                        sbnames = c("PasoTroncos", 
#                                    "FrayMarcos", "PasoPache"))
#

# test <- DistGR4J_objfun(x = x_in, calibrate_on = c(1:15,16,17,19,20),
#                         parM = par_in, 
#                         model_input = model_input_in, 
#                           objective = "r.squared")

# ----------------------------------------------------------
# Function to rewrite the parameters
rewrite_pars <- function(old_par, 
                         old_reach_par = NULL,
                         changed_par, sb = 3, which = NULL) {
  # old par is the list with parameters for subbasins
  # old_reach_par (optional) is a list reach parameters
  # changed_par is the output of the fit
  # sb (optional) number of subbasins
  # which (optional) if only a single sb, which sb
  # browser()
  # loop through subbasins
  for (i in 1:sb) {
    #browser()
    # calculate number of parameters
    if (length(old_reach_par)>0) {
      if (sb == 1) {
        no_par <- (length(changed_par) - 2)
      } else {
        no_par <- (length(changed_par) - 2*(sb-1))/sb
      }
    } else no_par <- (length(changed_par))/sb
    # change the subbasin parameters
    if (sb==1) {
      for (j in 1:length(unique(names(changed_par)
                                [1:(sb*no_par)]))){
          old_par[[sb]][grep(names(changed_par)[j],
                         names(old_par[[sb]]))] <- changed_par[[j]]
      }
    } else {
      for (j in 1:length(unique(names(changed_par)
                                [1:(sb*no_par)]))){
        old_par[[i]][grep(names(changed_par)[j+(i-1)*sb],
                names(old_par[[i]]))] <- changed_par[[j+(i-1)*sb]]
      }
    }
    # change the reach parameters
    if (length(old_reach_par)>0) {
      if (sb==1 & length(which) > 0) {
        for (j in 1:length(unique(names(changed_par)
                                  [((sb*no_par)+1):(length(changed_par))]))){
          old_reach_par[[sb]][grep(names(changed_par)
                                  [sb*no_par+j], names(old_reach_par[[sb]]))] <-
            changed_par[[sb*no_par+j]]
        }
      } else {
      if (i < sb) { # reaches are 1 fewer than sb
          for (j in 1:length(unique(names(changed_par)
                        [((sb*no_par)+1):(length(changed_par))]))){
            old_reach_par[[i]][grep(names(changed_par)
                [sb*no_par+j+(i-1)*(sb-1)], names(old_reach_par[[i]]))] <-
            changed_par[[sb*no_par+j+(i-1)*(sb-1)]]
          }
        }
      }  
    }
  }
  #browser()
  # return a list of the final parameters
  if(length(old_reach_par)>0) {
    return(list(sub = old_par,reach = old_reach_par))  
  } else {
    return(old_par)  
  }
}

#----------------------------------------------

#-------------------------------
# function to plot the results
# plotting function
plot_results <- function(Fitted_model, Data, plot=T) {
  # Fitted model is the model
  #browser()
  plot_df <- Fitted_model
  plot_df <- as.tibble(plot_df)
  # add the dates as a column to the data
  plot_df$fecha <- ymd(as.character(time(Fitted_model)))
  
  # Do the same with Data
  Data1 <- as.tibble(Data)
  Data1 <- Data1[,grep("Q",names(Data1))]
  colnames(Data1) <- paste("obs",colnames(Fitted_model),sep="_")
  # gather the observed data
  Data1 <- Data1 %>%
    gather(key = "obs_Subbasin", value = "mm_obs",
           obs_PasoTroncos:obs_PasoPache) 
  # gather the predicted data
  plot_df <- plot_df %>%
    gather(key = "Subbasin", value = "mm_pred" ,
           PasoTroncos:PasoPache)
    
  # add the Q data from the observed data
  plot_df <- cbind(plot_df, Data1)
  # change the column names for the observed data
  
  # make a plot to show pred and observed
  if (plot==T) {
    p <-  ggplot(plot_df,aes(fecha,mm_obs)) +
      geom_line(colour="red") + 
      geom_line(aes(fecha,mm_pred), colour="blue", alpha=0.5) + 
      facet_wrap(~Subbasin, ncol=1)
    print(p)
  }
return(plot_df) 
}
#-------------------------------------------------------

#------------------------------------
stats_fun <- function(res_data, decimal=2) {
  res_data <- as.tibble(res_data)
  stats <- res_data %>%
    dplyr::select(-(fecha)) %>%
    group_by(Subbasin) %>%
    summarise(rel.bias = round(hmadstat("rel.bias")(mm_obs,mm_pred),
                               decimal),
              r.squared = round(hmadstat("r.squared")(mm_obs,mm_pred),
                                decimal),
              r.sq.sqrt = round(hmadstat("r.sq.sqrt")(mm_obs,mm_pred),
                                decimal),
              r.sq.log = round(hmadstat("r.sq.log")(mm_obs,mm_pred),
                               decimal))
  return(stats)
}

# -----------------------------