# Utility function to plot ET data
# Willem Vervoort/Joseph Guillaume
# September 2015
# ---------------------------------------------------
plot.ET <- function(data,ModelFit, main="ETa predicted and observed",
                    xlab="Date", ylab="Actual ET (mm/day)", col="red",
                    lwd=2, lty=2,ylim=c(0,max(data$aET)+5)) {
  # calculate plotting time intervals
    plot.time <- unique(data$et.period)
  # aggregate modelled aET
    totals=aggregate(coredata(ModelFit$U$ET),
                   list(date=coredata(data$et.period)),sum)  
  if(require(tidyverse)==FALSE) {
    plot(data$aET[data$aET>0,],
         xlab=xlab, ylab=ylab, col=col,
         lwd=lwd, lty=lty,ylim=ylim, 
         main = main)
    lines(zoo(totals[,2],order.by=totals[,1]))
    legend("topleft",c("MODIS ET", "Predicted aET"),
           lwd=c(lwd,1),col=c(col,1),lty=c(lty,1))
  } else {
   # browser()
    plot.df <- data.frame(time = time(data$aET[data$aET>0,]), 
                          MODIS_ET = as.numeric(data$aET[data$aET>0]),
                          Predicted_aET = totals[,2])
    plot.df %>%
      gather(key = "ET", value="mm_ET", MODIS_ET,Predicted_aET) %>%
      ggplot(aes(time,mm_ET, colour = ET, linetype = ET)) + 
      geom_line(size=lwd) + xlab(xlab) + ylab(ylab) +
      ggtitle(main)
    }
}
