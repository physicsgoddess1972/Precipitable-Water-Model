#' :file: pmat_products.r
#' :module: Precipitable Water Model Analysis Tool: Products
#' :synopsis: plotting functions for PMAT
#' :author: Spencer Riley <sriley@pmat.app>

# Time series plots
time.pwindex <- function(datetime){
    #' :detail: Normalized PWV index for both clear sky and overcast data
    #' :param datetime: the datestamp of the data

    par(mar = c(3,4, 3, 3), oma = c(1, 1, 0, 0), mfrow=c(1, 1), xpd=TRUE)
    # Plotting margin
    range_over 		<- overcast.data$pw.index
    title 		    <- sprintf("Normalized Index for Weighted %s Time Series", pw_lab)
    range_clear	    <- clear_sky.data$pw.index

    datetime_clear <- as.POSIXct(paste(as.Date(unlist(clear_sky.data$date), origin="1970-01-01"),
                                       paste(unlist(clear_sky.data$time),":00", sep="")),
                                 format="%Y-%m-%d %H:%M:%S")
    datetime_over <- as.POSIXct(paste(as.Date(unlist(overcast.data$date), origin="1970-01-01"),
                                       paste(unlist(overcast.data$time),":00", sep="")),
                                 format="%Y-%m-%d %H:%M:%S")
    if (length(datetime_clear[!is.na(datetime_clear)]) > 0){
        x_pri <- datetime_clear; x_sec <- datetime_over
        y_pri <- unlist(range_clear); y_sec <- unlist(range_over)
    } else {
        x_pri <- datetime_over; x_sec <- datetime_clear
        y_pri <- unlist(range_over); y_sec <- unlist(range_clear)
    }

    # defines the max and min of the y-axis
    # plots the first range in the list
    plot(x_pri, y_pri,
         ylab="Normalized Index",
         xaxt='n',
         main=title,
         las =1,
         pch=16,
         ylim=c(0, 1),
         xlim=time_axis_init(datetime)[[1]])

    time_axis(datetime)
    # plots all other ranges in the list if the length of range is greater than 1
    if (length(x_sec[!is.na(x_sec)]) > 0){
        points(x_sec, y_sec, pch=16)
    }
    # print statement when completed
    logg("PASS",title, lev = level)
}
time.nth_range <- function(y, title, color, leg.lab, ylab, datetime, over.bool){
    #' :detail: Multirange Time Series plot series
    #' :param list y: the range of the plot
    #' :param character title: the title/description of the plot
    #' :param character color: the color string for the data range
    #' :param list leg.lab: a list of
    #' :param character ylab: the y-axis label
    #' :param double datetime: the datetime array
    #' :param logical over.bool: the condition of data (clear sky/overcast)
    #print(datetime)
    test <- unlist(y)
    test[!is.finite(unlist(y))] <- NA
    # defines the max and min of the y-axis
    ymin <- min(as.numeric(unlist(test)), na.rm=TRUE)
    ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
    plot(datetime, y[[1]],
         ylab=ylab[1],
         xaxt='n',
         las =1,
         xlab='',
         main=stnd_title(title, over.bool),
         pch=16,
         ylim=c(ymin, ymax),
         xlim=time_axis_init(datetime)[[1]],
         col=color[1])
    minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
    time_axis(datetime)
    # plots all other ranges in the list if the length of range is greater than 1
    if (length(y) > 1){
        for(j in 2:length(y)){
            points(datetime, y[[j]], pch=16, col=color[j])
        }
    }
    # defines the legend
    if (!is.na(leg.lab)){
        legend("topright", inset=c(-0.21, 0),
                           legend=c(gsub("_", " ",leg.lab)),
                           col=c(color),
                           pch=c(16,16, 16))
    }
    # print statement when completed
    logg("PASS", title, lev = level)
}
time.composite <- function(y, title, color, ylab, datetime, over.bool){
    #' :detail: Time Series composite plot series
    #' :param list y: the range of the plot
    #' :param character title: the title/description of the plot
    #' :param character color: the color string for the data range
    #' :param character ylab: the y-axis label
    #' :param double datetime: the datetime array
    #' :param logical over.bool: the condition of data (clear sky/overcast)

    plot(datetime, y[[1]],
         ylab=ylab[[1]],
         col=color[[1]],
         main=stnd_title(title, over.bool),
         col.lab = color[[1]],
         xaxt='n',
         las =1,
         xlab=NA,
         pch=16,
         xlim=time_axis_init(datetime)[[1]])
    time_axis(datetime)

    par(new = T)
    plot(datetime, y[[2]],
         axes=F,
         xlab=NA,
         ylab=NA,
         col=color[[2]],
         pch=16,
         xlim=time_axis_init(datetime)[[1]])
    axis(side= 4, las=1); mtext(side=4, line=3, ylab[[2]], col=color[[2]])
#     minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list(side=4))
    logg("PASS", title, lev = level)
}
time.mono_composite <- function(y, title, ylab, datetime, over.bool){
    #' :detail: Time Series composite plot series
    #' :param list y: the range of the plot
    #' :param character title: the title/description of the plot
    #' :param character ylab: the y-axis label
    #' :param double datetime: the datetime array
    #' :param logical overcast: the condition of data (clear sky/overcast)

    plot(datetime, y[[1]],
         ylab=ylab[[1]],
         main=stnd_title(title, over.bool),
         xaxt='n',
         xlab=NA,
         pch=16,
         las = 1,
         xlim=time_axis_init(datetime)[[1]])
    mtext(side = 2, line=3.5, "\\#H0850", family="HersheySans", xpd=TRUE, cex=3)

    time_axis(datetime)

    par(new = T)
    plot(datetime, y[[2]], axes=F, xaxt='n',
         yaxt='n', ylab=NA, xlab=NA, col="black",
         pch=1,
         xlim=time_axis_init(datetime)[[1]])

    axis(side = 4, las=1);
    mtext(side = 4, line=3.5, "\\#H0905", family="HersheySans")#adj=0.38, padj=0.65, cex=3)
    mtext(side = 4, line=2.5, ylab[[2]])

    logg("PASS", title, lev = level)
}
time.multiyear <- function(y, title, color, datetime, ylab, over.bool){
    #' :detail: Climatology plot
    #' :param list y: the range of the plot
    #' :param character title: the title/description of the plot
    #' :param character color: the color string for the data range
    #' :param double datetime: the datetime array
    #' :param character ylab: the y-axis label
    #' :param logical overcast: the condition of data (clear sky/overcast)

    x <- unclass(as.POSIXlt(datetime))$yday
    dat <- with(data.frame(x=x,y=y),
                aggregate(list(y=y), list(x = x), mean))
    plot(dat$x, dat$y,
         pch=16,
         xlab="Day number",
         ylab=ylab,
         las = 1,
         xlim=c(1, 366),
         main=stnd_title(title, over.bool),
         col=color)
    minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
    logg("PASS", title)
}

# Analysis plots
analysis.nth_range <- function(over.bool, x, y, title, label, color, leg.lab){
  #' :detail: Comparative analysis plot
  #' :param logical overcast: the condition of data (clear sky/overcast)
  #' :param x: the domain of the plot
  #' :param y: the range of the plot
  #' :param title: the title/description of the plot
  #' :param label:
  #' :param color: the color string for the data
  #' :param leg.lab:

  ymax	<- max(unlist(y), na.rm=TRUE)
  ymin	<- min(unlist(y), na.rm=TRUE)
  col <- colscheme(y)

  plot(x,  y[[1]],
       xlab=label[[1]],
       ylab=label[[2]],
       ylim=c(ymin, ymax),
       main=stnd_title(title, over.bool),
       pch=16,
       col=color[[1]])
  minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

  if (length(y) >= 2){
      for(j in 2:length(y)){
          points(x, y[[j]], pch=16, col=color[[j]])
      }
  }
  if (!is.na(leg.lab)){
      legend("topright",  inset=c(-0.21, 0),
                          legend=leg.lab,
                          col=color,
                          pch=c(16,16, 16))
  }
  logg("PASS", title, lev = level)
}
analysis.regression	<- function(over.bool, x, y, title, label, iter){
    #' :detail: Super Average Plot with Exponential Fit
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :param double x: the domain of the plot
    #' :param double y: the range of the plot
    #' :param character title: the title/description of the plot
    #' :param list label: list containing the x-axis and y-axis labels
    #' :param list iter: the output of iter.analysis

    exp_reg <- exp.regression(mean.out=iter[["filter.mean"]])
    ymax	<- max(unlist(exp_reg$y), na.rm=TRUE)
    ymin	<- min(unlist(exp_reg$y), na.rm=TRUE)

    # Non-linear model (exponential)
    plot(x,  y,
         xlab=label[[1]],
         ylab=label[[2]],
         ylim=c(ymin, ymax),
         main=stnd_title(title, over.bool))
    # Best Fit
    curve(iter$A*exp(iter$B*x), col="black", add=TRUE)

    polygon(c(exp_reg$newx, rev(exp_reg$newx)),
            c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),
            col=rgb(0.25, 0.25, 0.25,0.25),
            border = NA)

    minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

    leg <- sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f*mm)", iter$A,
                                                         iter$B,
                                                         iter$S)
    legend("topleft",   col=c("black"),
                        lty=c(1),
                        legend=c(parse(text=leg)))

    logg("PASS", title, lev = level)
}
analysis.svm <- function(model){
    #' :detail: plots SVM
    #' :param list model: output of lsvm

    cf <- model$cf
    A <- -cf[2]/cf[3]
    title <- sprintf("SVM Analysis between Sky Temperature and %s", pw_lab)
    plot(model$x,model$y,
         pch=16, col=model$color,
         xlab="Temperature [C]",
         ylab=sprintf("log(%s)", pw_lab),
         main=title)
    curve(A*x + (-cf[1]/cf[3]), add=TRUE, col="black")
    curve(A*x + (-(cf[1] + 1)/cf[3]), col="black", add=TRUE, lty="dashed")
    curve(A*x + (-(cf[1] - 1)/cf[3]), col="black", add=TRUE, lty="dashed")
    minor.tick(nx=2, ny=2, tick.ratio=0.5,
               x.args = list(), y.args = list())

    leg <- sprintf("%.2f*x + %.2f",A, (-cf[1]/cf[3]))
	legend("topleft",
           col=c("black", "blue", "red"),
           pch=c(NA, 16, 16),
           lty=c(1, NA, NA),
           legend=c(parse(text=leg), "clear-sky", "overcast"))
    logg("PASS", title, lev = level)
  }

# Pacviz plots
pac.compare <- function(over.bool, title, x, y, angular, radial){
    #' :detail: Pac-Man plot of Super Average Plot
    #' :param logical overcast: the condition of data (clear sky/overcast)
    #' :param character title: the title/description of the plot
    #' :param double x: the domain of the plot
    #' :param double y: the range of the plot
    #' :param character angular: the angular axis label and units
    #' :param character radial: the radial axis label and units

    par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)

    # Finds and removes NaNed values from the dataset
    pac.plot(x,y,
             stnd_title(title, over.bool),
             angular,
             radial)

   logg("PASS", title, lev = level)
}
pac.regression <- function(over.bool){
    #' :detail: Pac-Man residual plot
    #' :param bool over.bool: the condition of data (clear sky/overcast)

    ifelse(over.bool, x <- overcast.data$snsr_sky_calc,
                     x <- clear_sky.data$snsr_sky_calc)
    ifelse(over.bool, y <- log(overcast.data$avg, base=exp(1)),
                     y <- log(clear_sky.data$avg, base=exp(1)))
    title <- sprintf("Pac-Man Residual of the Mean %s and Temperature Model", pw_lab)

    # Finds and removes NaNed values from the dataset
    nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
    if (length(nans) > 0){
        x <- x[-(nans)]; y <- y[-(nans)]
    }
    pac.resid(x, y,
              stnd_title(title, over.bool),
              c("Zenith Sky Temperature", "degC"))

    logg("PASS", title, lev = level)
}

chart.histogram <- function(range, xlabel, title){
        #' :detail: Histograms of defined quantities
        #' :param double range: the range of the plot
        #' :param list xlabel: the x-axis label of the plot
        #' :param list title: the title/description of the histogram

        h <- hist(range,
                  main = paste("Distribution of", title, sep=" "),
                  prob = FALSE,
                  xlab = xlabel,
                  xlim=c(floor(min(range, na.rm = TRUE)),
                          ceiling(max(range, na.rm = TRUE))),
                  ylab = "Number of Occurances")
        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
        text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5))

        x <- seq(min(range, na.rm = TRUE),
                 max(range, na.rm = TRUE), length = 40)

        f <- (h$counts / h$density) * dnorm(x, mean = mean(range, na.rm = TRUE), sd = sd(range, na.rm=TRUE))
        logg("PASS", sprintf("%s", title), lev = level)
  }

poster.plots <- function(over.bool, iter, mean.out){
    #' :detail: The set of all poster
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :param list iter: output of iterative.analysis
    #' :param list mean.out:
    #' :return: All available poster plots
#     print(list(typeof(iter), typeof(mean.out)))
    force(date); force(over.bool)
    for (i in 1:length(snsr_name)){
        if(config[[1]]$instruments[[1]]$sensor$poster == FALSE){
            snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
            snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
            snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
            snsr_name[[i]] <- NULL
        }
    }

    poster1 <- function(...){
        #' :details: The time series poster plot

        par(mfrow=c(3,2),mar=c(1,2, 3.1, 1), oma=c(1,2,0,0), xpd=TRUE)
        xmin <- min(do.call("c", clear_sky.data$date), na.rm=TRUE)
        xmax <- max(do.call("c", clear_sky.data$date), na.rm=TRUE)

        ymax <- max(c(as.numeric(unlist(overcast.data$snsr_sky)),
                      as.numeric(unlist(clear_sky.data$snsr_sky))), na.rm=TRUE)
        ymin <- min(c(as.numeric(unlist(overcast.data$snsr_sky)),
                      as.numeric(unlist(clear_sky.data$snsr_sky))), na.rm=TRUE)

        range_index <- clear_sky.data$snsr_sky
        clear_date <- clear_sky.data$date
        over_date <- overcast.data$date

        plot(clear_date,range_index[[1]],xlab=NA,
                                         ylab=NA,
                                         main=NA,
                                         pch=16,
                                         xaxt='n',
                                         xlim=c(xmin, xmax),
                                         ylim=c(ymin, ymax),
                                         col=c(snsr_color[1]),
                                         las=1)
        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 4),"-01-01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        mtext("Sky Temperature",line=0.5, cex=0.75)
        mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(clear_date,range_index[[j]], pch=16,
                col=c(snsr_color[j]))
            }
        }
        legend("topleft", legend=c(gsub("_", " ", snsr_name)),col=snsr_color, pch=16)

        range_index <- overcast.data$snsr_sky

        plot(over_date,range_index[[1]], ylab=NA,
                                         main=NA,
                                         pch=16,
                                         las=1,
                                         col=snsr_color[1],
                                         xaxt='n',
                                         xlim=c(xmin, xmax),
                                         ylim=c(ymin, ymax))

       ticks.at <- seq(as.Date(paste(substr(over_date[[1]], 1, 4),"-01-01",sep="")), over_date[[length(over_date)]] , by = "months")

        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        mtext("Sky Temperature", line=0.5, cex=0.75)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(over_date, range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        ymax  		<- max(c(as.numeric(unlist(clear_sky.data$snsr_gro)),
                             as.numeric(unlist(clear_sky.data$snsr_gro))),na.rm=TRUE)
        ymin  		<- min(c(as.numeric(unlist(clear_sky.data$snsr_gro)),
                             as.numeric(unlist(overcast.data$snsr_gro))),na.rm=TRUE)
        range_index <- clear_sky.data$snsr_gro

        plot(clear_date, range_index[[1]], xlab=NA,
                                           ylab=NA,
                                           main=NA,
                                           pch=16,
                                           xaxt='n',
                                           xlim=c(xmin, xmax),
                                           ylim=c(ymin, ymax),
                                           col=snsr_color[1],
                                           las=1)

       ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 4),"-01-01",sep="")), clear_date[[length(clear_date)]] , by = "months")

        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        mtext("Ground Temperature", line=0.5, cex=0.75)
        mtext("Temperature [C]", side=2, line=2.5, cex=0.65)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(clear_date,range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        range_index <- overcast.data$snsr_gro

        plot(over_date, range_index[[1]], xlab=NA,
                                          ylab=NA,
                                          main=NA,
                                          pch=16,
                                          xaxt='n',
                                          xlim=c(xmin, xmax),
                                          ylim=c(ymin, ymax),
                                          col=snsr_color[1],
                                          las=1)

       ticks.at <- seq(as.Date(paste(substr(over_date[[1]], 1, 4),"-01-01",sep="")), over_date[[length(over_date)]] , by = "months")

        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        mtext("Ground Temperature", line=0.5, cex=0.75)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(over_date,range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        ymax 		<- max(c(as.numeric(unlist(clear_sky.data$snsr_del)),
                             as.numeric(unlist(overcast.data$snsr_del))), na.rm=TRUE)
        ymin 		<- min(c(as.numeric(unlist(clear_sky.data$snsr_del)),
                             as.numeric(unlist(overcast.data$snsr_del))), na.rm=TRUE)
        range_index <- clear_sky.data$snsr_del

        plot(clear_date,range_index[[1]], xlab=NA,
                                          ylab=NA,
                                          main=NA,
                                          pch=16,
                                          xaxt='n',
                                          xlim=c(xmin, xmax),
                                          ylim=c(ymin, ymax),
                                          col=snsr_color[1],
                                          las=1)

       ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 4),"-01-01",sep="")), clear_date[[length(clear_date)]] , by = "months")

        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        mtext("Difference in Temperature", line=0.5, cex=0.75)
        mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(clear_date, range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        range_index <- overcast.data$snsr_del

        plot(over_date,range_index[[1]], xlab=NA,
                                         ylab=NA,
                                         main=NA,
                                         pch=16,
                                         xaxt='n',
                                         xlim=c(xmin, xmax),
                                         ylim=c(ymin, ymax),
                                         col=snsr_color[1],
                                         las=1)

        ticks.at <- seq(as.Date(paste(substr(over_date[[1]], 1, 4),"-01-01",sep="")), over_date[[length(over_date)]] , by = "months")

        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        mtext("Difference in Temperature", line=0.5, cex=0.75)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(over_date,range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        mtext("Condition: Overcast", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.76))
        mtext("Condition: Clear Sky", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.26))
        logg("PASS", "Sky-Ground-Delta Temperature Time Series", lev = level)
    }

    poster2 <- function(over.bool, iter, mean.out){
        #' :detail: The analytics poster plot
        #' :param logical overcast: the condition of data (clear sky/overcast)
        #' :param iter: the output of iterative.analysis
        #' :param mean.out:

        ## Layout/Margin Configuration
            par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
            layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
        ## Locational Averagen PW Temperature Correlation
            if (over.bool){
                xmax 	<- max(as.numeric(unlist(overcast.data$snsr_sky_calc)), na.rm=TRUE)
                xmin 	<- min(as.numeric(unlist(overcast.data$snsr_sky_calc)), na.rm=TRUE)
                x 		<- as.numeric(unlist(overcast.data$snsr_sky_calc))

                range 	<- overcast.data$tmp_avg
            } else {
                xmax 	<- max(as.numeric(unlist(clear_sky.data$snsr_sky_calc)), na.rm=TRUE)
                xmin 	<- min(as.numeric(unlist(clear_sky.data$snsr_sky_calc)), na.rm=TRUE)
                x 		<- as.numeric(unlist(clear_sky.data$snsr_sky_calc))

                range 	<- clear_sky.data$tmp_avg
            }
            ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE)
            ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)

            plot(x, t(unlist(range[1])), xlab=NA,
                                         ylab=NA,
                                         xlim=c(xmin, xmax),
                                         ylim=c(ymin, ymax),
                                         main=NA,
                                         pch=16,
                                         col=colscheme(range)[1])

            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            mtext("Locational Mean TPW and Temp",line=0.5)

            mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
            if (length(range) >= 2){
                for(j in 2:length(range)){
                    points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
                }
            }
            legend("topleft", legend=paste(unique(pw_time), "Z"), col=colscheme(range), pch=c(16))

        ## Temporal Average Pw Temperature Correlation
            if (over.bool){
                range 	<- overcast.data$loc_avg
            } else {
                range 	<- clear_sky.data$loc_avg
            }
            exp_reg <- exp.regression(mean.out = mean.out)
            ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE)
            ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)

            plot(x,  t(unlist(range[1])), xlab=NA,
                                          ylab=NA,
                                          xlim=c(xmin, xmax),
                                          ylim=c(ymin, ymax),
                                          main=NA,
                                          pch=16,
                                          col=colscheme(range)[1])

            mtext("Temporal Mean TPW and Temp",line=0.5)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            if (length(range) >= 2){
                for(j in 2:length(range)){
                    points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
                }
            }
            legend("topleft", legend=unique(pw_place), col=colscheme(range), pch=16)

        ## Total Mean PW Temperature Correlation with exponential regression
        # Non-linear model (exponential)
            plot(exp_reg$x,exp_reg$y, pch=1,
                                      xlim=c(exp_reg$xmin, exp_reg$xmax),
                                      xlab=NA,
                                      ylab=NA,
                                      main=NA)

            mtext("Mean TPW vs Temp",line=0.5)
            mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
        # Best Fit
        curve(iter$A*exp(iter$B*x), col="black", add=TRUE)

        # Prediction Interval
            polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]),
                      rev(exp(exp_reg$predint[ ,2]))),
                      col=rgb(0.25, 0.25, 0.25,0.25),
                      border = NA)
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        legend("topleft",col=c("black"), lty=c(1),
        legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f*mm)",
        iter$A,iter$B,iter$S))))
        # Layout configuration for preceding plots
        layout(matrix(c(1), 2, 2, byrow=TRUE))
        logg("PASS", "Analytical Plots", lev = level)
    }

    return(list(poster1(),
                poster2(over.bool, iter, mean.out)))
}

# Sensor specific plots
sensor.chart <- function(...){
    #' :detail: sensor specific data type distribution charts

    par(mar=c(5.1, 5.1, 5.1, 1.3),oma=c(0,0,0,0), xpd=TRUE)
    snsr_sky  <- clear_sky.data$raw_sky
    snsr_skyo <- overcast.data$raw_sky

    for (count in 1:length(snsr_name)){
            test <- unlist(snsr_sky[count])
            testo <- unlist(snsr_skyo[count])
            testo[is.finite(testo)] <- NA
            test[is.finite(test)] <- NA

            norm_inf <- length(na.omit(test))
            over_inf <- length(na.omit(testo))

            norm	<- length(na.omit(unlist(snsr_sky[count]))) - norm_inf
            over	<- length(na.omit(unlist(snsr_skyo[count]))) - over_inf

            norm_na <- length(unlist(snsr_sky[count])) - norm - norm_inf
            over_na <- length(unlist(snsr_skyo[count])) - over - over_inf

            slices  <- data.frame(B=c(over, over_na, over_inf),A=c(norm,norm_na, norm_inf))
            title 	<- c("Overcast","Clear Sky")

            over_pct <- c(over/length(unlist(snsr_skyo[count]))*100,
                          over_na/length(unlist(snsr_skyo[count]))*100,
                          over_inf/length(unlist(snsr_skyo[count]))*100)

            clear_pct <- c(norm/length(unlist(snsr_sky[count]))*100,
                           norm_na/length(unlist(snsr_sky[count]))*100,
                           norm_inf/length(unlist(snsr_sky[count]))*100)

            pct 	<- data.frame(B=c(ifelse(is.na(over_pct[1]), 0, over_pct[1]),
                                      ifelse(is.na(over_pct[2]), 0, over_pct[2]),
                                      ifelse(is.na(over_pct[3]), 0, over_pct[3])),
                                  A=c(ifelse(is.na(clear_pct[1]), 0, clear_pct[1]),
                                      ifelse(is.na(clear_pct[2]), 0, clear_pct[2]),
                                      ifelse(is.na(clear_pct[3]), 0, clear_pct[3])))

            bar <- barplot(as.matrix(slices),names.arg=title, las=1,
                           ylim=c(0,round(max(as.matrix(slices)+25, -2))),
                           horiz=FALSE,
                           ylab="Samples",
                           density=c(0, 25, 25),
                           angle=c(0, 45, 135),
                           axes=FALSE,
                           beside=TRUE,
                           main=NA,
                           col="black")
            mtext(sprintf("Data Type Distribution: %s", gsub("_", " ",snsr_name[count])), side=3, line=3, cex=1)
            legend("top",
                    inset=c(0,-0.1),
                    legend = c("Decimal", "NaN", "-Inf"),
                    density=c(0, 25, 25),
                    angle=c(0, 45, 135),
                    bty = "n",
                    y.intersp = 2,
                    ncol=3,
                    cex=1)
            axis(side = 2, labels=TRUE, las=1)
            minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
            for (i in 1:3){
                for (j in 1:length(pct)){
                    text(bar[i,j],as.numeric(as.matrix(slices)[i,j]), pos=3, xpd=NA,
                                labels=sprintf('%s %%', round(as.numeric(pct[i,j]),1)))
                }

            }
        logg("PASS", sprintf("Overcast Condition Percentage: %s", gsub("_", " ",snsr_name[count])), lev = level)
    }
}
sensor.time <- function(over.bool){
        #' :detail: Instrumentation time series plots
        #' :param logical over.bool: the condition of data (clear sky/overcast)

        # X axis limits
        for (count in col_snsr){
            par(mar=c(3,4, 3, 1), oma=c(1,1,0,0), xpd=FALSE)
            layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
            if(over.bool){
                date 		<- overcast.data$date
                time        <- overcast.data$time
                snsr_sky    <- overcast.data$snsr_sky
                snsr_gro    <- overcast.data$snsr_gro
                sky_title 	<- sprintf("Condition: Overcast \n Sky Temperature Time Series for %s", snsr_tag[count[1]])
                gro_title 	<- sprintf("Ground Temperature Time Series for %s", snsr_tag[count[1]])
            }else{
                date 		<- clear_sky.data$date
                time        <- clear_sky.data$time
                snsr_sky    <- clear_sky.data$snsr_sky
                snsr_gro    <- clear_sky.data$snsr_gro
                sky_title 	<- sprintf("Condition: Clear Sky \n Sky Temperature Time Series for %s", snsr_tag[count[1]])
                gro_title 	<- sprintf("Ground Temperature Time Series for %s", snsr_tag[count[1]])
            }
            datetime <- as.POSIXct(paste(as.Date(unlist(date), origin="1970-01-01"),
                                 paste(unlist(time),":00", sep="")),
                           format="%Y-%m-%d %H:%M:%S")
            sky_range <- snsr_sky[count]
            gro_range <- snsr_gro[count]

            plot(datetime, t(unlist(sky_range[1])),
                 xlab="Date",
                 ylab="Temperature [C]",
                 main=sky_title,
                 pch=16,
                 xaxt='n',
                 xlim=time_axis_init(datetime)[[1]],
                 ylim=c(min(unlist(sky_range), na.rm=TRUE), max(unlist(sky_range), na.rm=TRUE)),
                 col=c(snsr_color[count[1]]))
            minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            time_axis(datetime)

            if (length(sky_range) > 1){
                for(j in 2:length(sky_range)){
                    points(date,t(unlist(sky_range[j])), pch=16, col=c(snsr_color[count[j]]))
                }
                legend("topleft",col=c(snsr_color[count]), pch=16,
                    legend=c(gsub("_", " ",snsr_name[count])))
            }
            plot(datetime, t(unlist(gro_range[1])),
                 xlab="Date",
                 ylab="Temperature [C]",
                 main=gro_title,
                 pch=16,
                 xaxt='n',
                 xlim=time_axis_init(datetime)[[1]],
                 ylim=c(min(unlist(gro_range), na.rm=TRUE), max(unlist(gro_range), na.rm=TRUE)),
                 col=c(snsr_color[count[1]]))
            minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            time_axis(datetime)

            if (length(gro_range) > 1){
                for(j in 2:length(gro_range)){
                    points(datetime,t(unlist(gro_range[j])), pch=16, col=c(snsr_color[count[j]]))
                }
            }
            logg("PASS", sprintf("Sky-Ground Time Series: %s", gsub("_", " ",unique(snsr_tag)[count][1])), lev = level)
        }
    }

## Data products
data.gen <- function(over.bool, dir){
        #' :detail: creates a datafile containing the date, avg temp, and avg pwv for a defined condition
        #' :param bool over.bool: the condition of the data (clear sky/overcast)
        #' :param dir: directory path

       ifelse(over.bool, res <- overcast.data,
                        res <- clear_sky.data)
      # Pulls the data
        avg_temp  <- res$snsr_sky_calc
        avg_pw 	  <- res$avg
        date      <- res$date
        class(date) <- "Date"
      # Pulls the data
        norm  	  <- data.frame(list(x=date, y1=avg_temp, y2=avg_pw))
      # Removes the NaN data
        if (length(c(which(avg_pw %in% NaN))) != 0){
          norm 	  <- norm[-c(which(avg_pw %in% NaN)), ]
        }
        if (length(c(which(avg_temp %in% NaN))) != 0){
          norm 	  <- norm[-c(which(avg_temp %in% NaN)), ]
        }
      # Adds data to a data frame with column names
        data 	  <- data.frame(list(date=c(norm$x),
                                      avg_temp=c(norm$y1),
                                     avg_pw=c(norm$y2)))

        colnames(data) <- c("date", "avg_temp", "avg_pw")
      # Writes the data to a csv

        write.csv(data,
                  file=file.path(dir,
                                 sprintf("data%s.csv",
                                         ifelse(over.bool,"_overcast", ""))),
                  row.names=FALSE)
        logg("PASS", sprintf("Data sent to data/data%s.csv", ifelse(over.bool,"_overcast", "")), lev = level)
}
data.ml <- function(out.dir){
      #' :detail: creates a datafile containing the machine learning relavant information
      #' :param character out.dir: directory path

        ml_pw <- ml_pw_avg <- ml_temp <- ml_temp_avg <- ml_rh <- list()
        ## Average PW
        for(a in 1:length(col_pw)){
          ml_pw[[ paste("ml_pw", a, sep="") ]] <- as.numeric(unlist(fname[col_pw[a]]))
        }
        for(a in ml_pw){
          for(b in 1:(length(unlist(ml_pw))/length(ml_pw))){
              ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]] <- append(x=ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]], value=na.omit(c(a[b])))
          }
        }
        for(a in 1:(length(unlist(ml_pw))/length(ml_pw))){
          ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]] <- mean(ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]])
        }
        # Average Temperature
        for(a in 1:length(col_sky)){
          ml_temp[[ paste("ml_temp", a, sep="") ]] <- as.numeric(unlist(fname[col_sky[a]]))
          ml_temp[[ paste("ml_temp", a, sep="") ]] <- replace(ml_temp[[ paste("ml_temp", a, sep="") ]], ml_temp[[ paste("ml_temp", a, sep="") ]] == "-Inf", NaN)
        }

        for(a in ml_temp){
          for(b in 1:(length(unlist(ml_temp))/length(ml_temp))){
              ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]] <- append(x=ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]], value=na.omit(c(a[b])))
          }
        }
        for(a in 1:(length(unlist(ml_temp))/length(ml_temp))){
          ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]] <- mean(ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]])
        }
        ## Relative Humidity
        for(a in 1:length(col_rh)){
          ml_rh[[ paste("ml_rh", a, sep="") ]] <- as.numeric(unlist(fname[col_rh[a]]))
        }
        # Pulls the data
        avg_temp	<- as.numeric(unlist(ml_temp_avg))
        avg_pw 		<- as.numeric(unlist(ml_pw_avg))
        avg_rh 		<- as.numeric(unlist(ml_rh))
        date 		<- as.Date(fname[ ,col_date], "%Y-%m-%d")
        cond 		<- fname[,col_con]
        # Pulls the data
        norm  		<- na.omit(data.frame(list(x=date, y1=avg_temp, y2=avg_pw, y3=avg_rh, c=cond)))
        data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2), avg_rh=c(norm$y3), cond=c(norm$c)))
        colnames(data) <- c("date", "avg_temp", "avg_pw", "avg_rh", "condition")
        # Writes the data to a csv
        write.csv(data, file=file.path(out.dir, "ml_data.csv"), row.names=FALSE)
        logg("PASS", sprintf("Data sent to %sml_data.csv", out.dir), lev = level)
}
data.step <- function(seed, i, coef, r, S){
    #' :detail: writes a yaml object that contains the analysis results of each step in the iterative analysis
    #' :param integer seed: the generated seed
    #' :param integer i:  the step number
    #' :param list coef: the coefficients of the best-fit
    #' :param double r: the RSME value
    #' :param double S: the standard deviation
    #' :return: a yaml object
    #' :rtype: list

    yml.out <- list(step=c(i),
                    seed=c(seed),
                    analysis=list(coeff=list(A=c(coef$A),
                                             B=c(coef$B)),
                                  rmse=c(r),
                                  rstd=c(S)))
    return(list(yml.out))
}
data.final <- function(out.dir, lengths, frac.kept, coef, std, rmse, over.bool=args$overcast){
    #' :detail: writes the final results of iterative.analysi
    #' :param character out.dir: the output directory
    #' :param list lengths: list of lengths
    #' :param list coef: the average coefficients of the best-fit
    #' :param double std: the average standard deviation
    #' :param double rmse: the average rsme values
    #' :param logical over.bool: the condition of data (clear sky/overcast)

    clear.len   = lengths[1]
    over.len    = lengths[2]
    train.len  = lengths[3]
    nan.len     = lengths[4]
    yml <- list(data=list(clear=list(total.count=c(clear.len)),
									overcast=list(total.count=c(over.len)),
									train.count=c(train.len),
									fraction.kept=c(frac.kept),
                                    nans=c(nan.len)),
							analysis=list(coeff=list(A=c(coef$A),
													 B=c(coef$B)),
                                      stdev=c(std),
                                      rmse=list(mims=c(rmse$M),
                                                   kelsey=c(rmse$K),
                                                   yours=c(rmse$R))))

  write(as.yaml(yml, precision=4),
        file = file.path(out.dir, sprintf("_results%s.yml", ifelse(over.bool,"_overcast", ""))))
}

## Plot function calls
visual.products <- function(set, mean.out, datetime=datetime, over.bool=args$overcast){
    #' :detail: saves plot sets
    #' :param character set: the --set parameter
    #' :param mean.out: output of mean.filter
    #' :param datetime: the datetime range of the data
    #' :param logical over.bool: the condition of data (clear sky/overcast)

	if(set == "i"){
        logg("INFO", "Sensor Plot Set", lev = level)

		pdf(file.path(fig.dir,
                      sprintf("sensor%s.pdf",
                              ifelse(over.bool,"_overcast", ""))))

	    # plot function calls
        sensor.chart()
        sensor.time(over.bool)
        return(NULL)
	}else if(set == "t"){
        logg("INFO", "Time Series Plot Set", lev = level)
		pdf(file.path(fig.dir,
                      sprintf("timeseries%s.pdf",
                              ifelse(over.bool,"_overcast", ""))))
        # data inputs for time.nth_range
        range.r <- list(res$snsr_sky,
                       res$snsr_gro,
                       res$snsr_del,
                       res$pw_loc,
                       res$tmp_avg,
                       res$loc_avg,
                       list(res$avg),
                       list(res$dew))
        range.t <- list("Sky Temperature Time Series",
                       "Ground Temperature Time Series",
                       "Difference Between Ground-Sky Temperature Time Series",
                       sprintf("%s Time Series", pw_lab),
                       sprintf("Temporal Mean %s Time Series", pw_lab),
                       sprintf("Locational Mean %s Time Series", pw_lab),
                       sprintf("Spatiotemporal Mean %s Time Series", pw_lab),
                       "Dewpoint Temperature Time Series")

        range.col <- unlist(list(rep(list(snsr_color), 3),
                          rep(list(pw_color), 3), "blue", "black"),
                     recursive=FALSE)
        range.leg <- unlist(list(rep(list(snsr_name), 3),
                          list(pw_name),
                          list(unique(pw_place)),
                          list(paste(unique(pw_time), "Z")), NA, NA),
                     recursive=FALSE)
        range.ylab <- unlist(list(rep(list("Temperature [C]"), 3),
                          rep(list(sprintf("%s [mm]", pw_lab)), 4), "Temperature [C]"),
                     recursive=FALSE)

        # data inputs for time.composite
        composite.r <- list(list(res$snsr_sky_calc, res$avg),
                           list(res$snsr_sky_calc, res$rh),
                           list(res$avg, res$rh),
                           list(res$avg, res$dewpoint))

        composite.title <- list(sprintf("Mean Sky Temperature and %s Time Series", pw_lab),
                                "Mean Sky Temperature and RH Time Series",
                               sprintf("Mean %s and RH Time Series", pw_lab),
                               sprintf("Mean %s and Dewpoint Time Series", pw_lab))
        composite.col <- list(list("red", "blue"),
                   list("red", "darkolivegreen3"),
                   list("blue", "darkolivegreen3"),
                   list("blue", "darkorange"))
        composite.ylab <- list(list("Temperature [C]", sprintf("%s [mm]", pw_lab)),
                               list("Temperature [C]", "RH [%]"),
                               list(sprintf("%s [mm]", pw_lab), "RH [%]"),
                               list(sprintf("%s [mm]", pw_lab), "Dewpoint Temperature [C]"))
        composite_mono.r <- list(list(res$snsr_sky_calc, res$avg),
                           list(res$snsr_sky_calc, res$rh),
                           list(res$avg, res$rh),
                           list(res$avg, res$dewpoint))
        composite_mono.title <- list(sprintf("Mean Sky Temperature and %s Time Series", pw_lab),
                                "Mean Sky Temperature and RH Time Series",
                               sprintf("Mean %s and RH Time Series", pw_lab),
                               sprintf("Mean %s and Dewpoint Time Series", pw_lab))
        composite_mono.ylab <- list(list("Temperature [C]", sprintf("%s [mm]", pw_lab)),
                               list("Temperature [C]", "RH [%]"),
                               list(sprintf("%s [mm]", pw_lab), "RH [%]"),
                               list(sprintf("%s [mm]", pw_lab), "Dewpoint Temperature [C]"))
        # data inputs for time.multiyear
        multiyear.r <- list(res$snsr_sky_calc, res$dewpoint)
        multiyear.title <- list("Climatology of mean sky temperature",
                                 "Climatology of dewpoint temperature")
        multiyear.col <- list("black", "black")
        multiyear.ylab <- list("Temperature [C]", "Dewpoint Temperature [C]")
        if (length(datetime) > 0){
            # plot function calls
            par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
            
            for (i in 1:length(range.r)){
                if (length(range.r[[i]][[1]]) > 0){
                    time.nth_range(range.r[[i]],
                                range.t[[i]],
                                range.col[[i]],
                                range.leg[[i]],
                                range.ylab[[i]],
                                datetime, over.bool)
                }
            }
            for (i in 1:length(composite.r)){
                if (length(composite.r[[i]][[1]]) > 0 && length(composite.r[[i]][[2]]) > 0){
                    time.composite(composite.r[[i]],
                                composite.title[[i]],
                                composite.col[[i]],
                                composite.ylab[[i]],
                                datetime, over.bool)
                }
            }
            for (i in 1:length(composite_mono.r)){
                if (length(composite.r[[i]][[1]]) > 0 && length(composite.r[[i]][[2]]) > 0){
                    time.mono_composite(composite_mono.r[[i]],
                                composite_mono.title[[i]],
                                composite_mono.ylab[[i]], datetime, over.bool)
                }
            }
            par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=TRUE)
            for (i in 1:length(multiyear.r)){
                if (length(multiyear.r[[i]]) > 0){
                    time.multiyear(multiyear.r[[i]],
                                    multiyear.title[[i]],
                                    multiyear.col[[i]],
                                    datetime, multiyear.ylab,
                                    over.bool)
                }
            }
            time.pwindex(datetime)
            return(NULL)
        } else {
            logg("ERROR", err_war$error$D[[1]]$code, lev = level)
            logg("ERROR", err_war$error$D[[1]]$fix, lev = level)
            aloha.closing()
        }
	}else if(set == "a"){
        logg("INFO", "Analytics Plot Set")
        pdf(file.path(fig.dir,
                      sprintf("analytics%s.pdf",
                              ifelse(over.bool,"_overcast", ""))))
        ifelse(over.bool, res <- overcast.data,
                         res <- clear_sky.data)
        # data inputs for analysis.nth_range
        x1 <- unlist(list(rep(list(res$snsr_sky_calc), 3)),
                     recursive=FALSE)
        y1 <- list(res$pw_loc, res$loc_avg, res$tmp_avg)
        t1 <- list(sprintf("Correlation between %s and Temperature", pw_lab),
                   sprintf("Correlation between Locational Mean %s and Temperature", pw_lab),
                   sprintf("Correlation between Temporal Mean %s and Temperature", pw_lab))
        l1 <- rep(list(list("Zenith Sky Temperature [C]", sprintf("%s [mm]", pw_lab))), 3)
        c1 <- unlist(list(rep(list(pw_color), 3)), recursive=FALSE)
        leg.lab <- unlist(list(list(pw_name),
                               list(unique(pw_place)),
                               list(paste(unique(pw_time), "Z"))),
                          recursive=FALSE)
        # data inputs for analysis.regression
        x2 <- unlist(list(rep(list(res$snsr_sky_calc), 2)),
                     recursive=FALSE)
        y2 <- list(res$avg, res$wt_avg)
        t2 <- list(sprintf("Regression between Spatiotemporal Mean %s and Temperature", pw_lab),
                   sprintf("Regression between Weighted %s and Temperature", pw_lab))
        l2 <- rep(list(list("Zenith Sky Temperature [C]", sprintf("%s [mm]", pw_lab))), 2)

        # data inputs for analysis.svm
        ml.x <- c(clear_sky.data$snsr_sky_calc, overcast.data$snsr_sky_calc)
        ml.y <- c(clear_sky.data$wt_avg, overcast.data$wt_avg)
        ml.l <- c(clear_sky.data$label, overcast.data$label)
        nan.ml <- nan.filter(list(x=ml.x, y=ml.y, l=ml.l))[[1]]
        ml <- lsvm(nan.ml$x, log(nan.ml$y, base=exp(1)), nan.ml$l)
        par(mar=c(5.1, 4.1, 4.1, 5.3), xpd=TRUE)
        # plot function calls
        for (i in 1:length(x1)){
            if (length(x1[[i]]) > 0 && length(y1[[i]]) > 0){
                analysis.nth_range(over.bool, x1[[i]], y1[[i]], t1[[i]], l1[[i]], c1[[i]], leg.lab[[i]])
            }
        }
        par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=FALSE)
        for (i in 1:length(x2)){
            if (length(x2[[i]]) > 0 && length(y2[[i]]) > 0){
                analysis.regression(over.bool, x2[[i]], y2[[i]], t2[[i]], l2[[i]], iter.results)
            }
        }
        par(mar=c(5.1, 4.1, 4.1, 2.1), xpd=FALSE)
        analysis.svm(ml)
	}else if(set == "c"){
        logg("INFO", "Chart Set", lev = level)
		pdf(file.path(fig.dir, "charts.pdf"))
        x <- list(c(cbind(overcast.data$wt_avg, clear_sky.data$wt_avg)),
                  cbind(unlist(overcast.data$snsr_sky_calc),
                         unlist(clear_sky.data$snsr_sky_calc)),
                  cbind(overcast.data$rh, clear_sky.data$rh),
                  cbind(overcast.data$dew, clear_sky.data$dew),
                  c(18.48 * exp(0.034 * clear_sky.data$snsr_sky_calc)))

        xlab <- list(sprintf("%s [mm]", pw_lab),
                     "Temperature [C]",
                    "Relative Humidity [%]",
                    "Temperature [C]",
                    sprintf("%s [mm]", pw_lab))
        title <- list(sprintf("Weighted %s", pw_lab),
                      "Average Temperature",
                      "Relative Humidity",
                      "Dewpoint Temperature",
                      sprintf("Predicted clear sky %s from IR Product", pw_lab))

        for (i in 1:length(unique(pw_place))){
            x <- append(x, list(c(cbind(unlist(clear_sky.data$tmp_avg[i]),
                                        unlist(overcast.data$tmp_avg[i])))))
            xlab <- append(xlab, sprintf("%s [mm]", pw_lab))
            title <- append(title, sprintf("Distribution of %s in %s", pw_lab, unique(pw_place)[i]))
        }
        # plot function calls
        for (count in 1:length(x)){
          chart.histogram(x[[count]], xlab[count], title[count])
        }
        return(NULL)
	} else if (set == "p") {
        logg("INFO", "Pac-Man Plot Set", lev = level)
		pdf(file.path(fig.dir,
                      sprintf("pacman%s.pdf",
                              ifelse(over.bool,"_overcast", ""))))
        exp.reg <-  exp.regression(tr.sz, mean.out)
        x1 <- list(exp.reg$x)
        y1 <- list(exp.reg$y)
        t1 <- list(sprintf("Correlation between Spatiotemporal Mean %s and Temperature", pw_lab))
        th1 <- list(c("Zenith Sky Temperature", "C"))
        ra1 <- list(c(sprintf("%s", pw_lab), "mm"))

        # plot function calls
        for (i in 1:length(x1)){
          pac.compare(over.bool, t1[[i]], x1[[i]], y1[[i]], th1[[i]], ra1[[i]])
        }
        pac.regression(over.bool)
        return(NULL)
	} else if (set == "o"){
        logg("INFO", "Poster Plot Set", lev = level)
		pdf(file.path(fig.dir,
                      sprintf("poster%s.pdf",
                              ifelse(over.bool,"_overcast", ""))))
        # plot function calls
        poster.plots(over.bool, iter.results, mean.out)
        return(NULL)
	}
}
