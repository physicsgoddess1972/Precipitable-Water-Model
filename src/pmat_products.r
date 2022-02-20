#' :file: pmat_products.r
#' :module: Precipitable Water Model Analysis Tool: Products
#' :synopsis: plotting functions for PMAT
#' :author: Spencer Riley <sriley@pmat.app>

time.pwindex <- function(datetime){
    #' :detail: Normalized PWV index for both clear sky and overcast data
    #' :param date: the datestamp of the data

    par(mar = c(3,4, 3, 3), oma = c(1, 1, 0, 0), mfrow=c(1, 1), xpd=TRUE)
    # Plotting margin
    range_over 		<- overcast.data$pw.index
    title 		    <- sprintf("Normalized Index for Weighted PWV Time Series")
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
         pch=16,
         ylim=c(0, 1),
         xlim=time_axis_init(datetime)[[1]])

    time_axis(datetime)
    # plots all other ranges in the list if the length of range is greater than 1
    if (length(x_sec[!is.na(x_sec)]) > 0){
        points(x_sec, y_sec, pch=16)
    }
    # print statement when completed
    logg("PASS","Normalized Index for Weighted PWV Time Series")
}

time.nth_range <- function(range, title, color, leg.lab, ylab, datetime,overcast){
    #' :detail: Multirange Time Series plot series
    #' :param date: the datestamp of the data
    #' :param bool overcast: the condition of data (clear sky/overcast)
    test <- unlist(range)
    test[!is.finite(unlist(range))] <- NA
    # defines the max and min of the y-axis
    ymin <- min(as.numeric(unlist(test)), na.rm=TRUE)
    ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)

    plot(datetime, range[[1]],
         ylab=ylab[1],
         xaxt='n',
         main=stnd_title(title, overcast),
         pch=16,
         ylim=c(ymin, ymax),
         xlim=time_axis_init(datetime)[[1]],
         col=color[1])

    time_axis(datetime)
    # plots all other ranges in the list if the length of range is greater than 1
    if (length(range) > 1){
        for(j in 2:length(range)){
            points(datetime, range[[j]], pch=16, col=color[j])
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
    logg("PASS", title)
}

time.composite <- function(range, title, color, ylab, datetime, overcast){
    #' :detail: Time Series composite plot series
    #' :param date: the datestamp of the data
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :return: A sky temperature time series plot

    plot(datetime, range[[1]],
         ylab=NA,
         col=color[[1]],
         main=stnd_title(title, overcast),
         xaxt='n',
         pch=16,
         xlim=time_axis_init(datetime)[[1]])

    axis(side = 2); mtext(side = 2, line=3, ylab[[1]], col=color[[1]])

    time_axis(datetime)

    par(new = T)
    plot(datetime, range[[2]],
         ylab=NA,
         axes=F,
         xlab=NA,
         col=color[[2]],
         pch=16,
         xlim=time_axis_init(datetime)[[1]])
    axis(side = 4); mtext(side=4, line=3, ylab[[2]], col=color[[2]])
    logg("PASS", title)
}

analysis.nth_range <- function(overcast, x, y, title, label, color, leg.lab){
  #' :detail: Super Average Plot with Exponential Fit
  #' :param bool overcast: the condition of data (clear sky/overcast)
  #' :return: A sky temperature time series plot

  ymax	<- max(unlist(y), na.rm=TRUE)
  ymin	<- min(unlist(y), na.rm=TRUE)
  col <- colscheme(y)

  plot(x,  y[[1]],
       xlab=label[[1]],
       ylab=label[[2]],
       ylim=c(ymin, ymax),
       main=stnd_title(title, overcast),
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
  logg("PASS", title)
}

analysis.regression	<- function(overcast, x, y, des, label, iter, results){
    #' :detail: Super Average Plot with Exponential Fit
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :return: A sky temperature time series plot
    exp_reg <- exp.regression(results,
                              nan.out = iter[["nan.out"]],
                              data_indx = iter[["filter.mean"]])
    ymax	<- max(unlist(exp_reg$y), na.rm=TRUE)
    ymin	<- min(unlist(exp_reg$y), na.rm=TRUE)

    # Non-linear model (exponential)
    plot(x,  y,
         xlab=label[[1]],
         ylab=label[[2]],
         ylim=c(ymin, ymax),
         main=stnd_title(des, overcast))
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

    logg("PASS", des)
}

analysis.svm <- function(model){
    cf <- model$cf
    A <- -cf[2]/cf[3]
    plot(model$x,model$y,
         pch=16, col=model$color,
         xlab="Temperature [C]",
         ylab="log(TPW)",
         main="SVM Analysis between Sky Temperature and TPW")
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
    logg("PASS", "SVM Analysis between Sky Temperature and TPW")
  }

pac.compare <- function(overcast, des, x, y, angular, radial){
    #' :detail: Pac-Man plot of Super Average Plot
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :return: A sky temperature time series plot

    par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)

    # Finds and removes NaNed values from the dataset
    pac.plot(x,y,
             stnd_title(des, overcast),
             angular,
             radial)

   logg("PASS", des)
}

pac.regression <- function(overcast){
    #' :detail: Pac-Man residual plot
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :return: A sky temperature time series plot

    ifelse(overcast, x <- overcast.data$snsr_sky_calc,
                     x <- clear_sky.data$snsr_sky_calc)
    ifelse(overcast, y <- log(overcast.data$avg, base=exp(1)),
                     y <- log(clear_sky.data$avg, base=exp(1)))
    des <- "Pac-Man Residual of the Mean TPW and Temperature Model"

    # Finds and removes NaNed values from the dataset
    nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
    if (length(nans) > 0){
        x <- x[-(nans)]; y <- y[-(nans)]
    }
    pac.resid(x, y,
              stnd_title(des, overcast),
              c("Zenith Sky Temperature", "degC"))

    logg("PASS", des)
}

charts	<- function(...){
    #' :detail: A collection of histograms and charts
    #' :return: PDF of charts

    chart1 <- function(range, xlabel, title){
        #' :detail: Histograms of defined quantities
        #' :param range: a data range
        #' :param xlabel: the xaxis label
        #' :param title: the title of the histogram

        for (count in 1:length(range)){
            h <- hist(range[[count]],
                      main = paste("Distribution of", title[count], sep=" "),
                      prob = FALSE,
                      xlab = xlabel[count],
                      xlim=c(floor(min(range[[count]], na.rm = TRUE)),
                              ceiling(max(range[[count]], na.rm = TRUE))),
                      ylab = "Number of Occurances")
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
            text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5))

            x <- seq(min(range[[count]], na.rm = TRUE),
                     max(range[[count]], na.rm = TRUE), length = 40)

            f <- (h$counts / h$density) * dnorm(x, mean = mean(range[[count]], na.rm = TRUE), sd = sd(range[[count]], na.rm=TRUE))
            lines(x,f, type="l", col="black", lwd=2, lty=1)
            logg("PASS", sprintf("%s", title[count]))
        }

    }
    x <- list(c(cbind(overcast.data$wt_avg, clear_sky.data$wt_avg)),
              cbind(unlist(overcast.data$snsr_sky_calc),
                     unlist(clear_sky.data$snsr_sky_calc)),
              cbind(overcast.data$rh, clear_sky.data$rh),
              cbind(overcast.data$dew, clear_sky.data$dew))

    xlab <- list("Precipitable Water [mm]",
                 "Temperature [C]",
                "Relative Humidity [%]",
                "Temperature [C]")
    title <- list("Weighted PWV",
                  "Average Temperature",
                  "Relative Humidity",
                  "Dewpoint Temperature")

    for (i in 1:length(unique(pw_place))){
        x <- append(x, list(c(cbind(unlist(clear_sky.data$tmp_avg[i]),
                                    unlist(overcast.data$tmp_avg[i])))))
        xlab <- append(xlab, "Precipitable Water [mm]")
        title <- append(title, sprintf("Distribution of PWV in %s", unique(pw_place)[i]))
    }
    return(list(chart1(x, xlab, title)))
}

poster.plots <- function(overcast, iter, nan.out, mean.out){
    #' :detail: The set of all poster
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :return: All available poster plots

    force(date); force(overcast)
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
        logg("PASS", "Sky-Ground-Delta Temperature Time Series")
    }

    poster2 <- function(overcast, iter, nan.out, mean.out){
        #' :detail: The analytics poster plot
        #' :param bool overcast: the condition of data (clear sky/overcast)

        ## Layout/Margin Configuration
            par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
            layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
        ## Locational Averagen PW Temperature Correlation
            if (overcast){
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
            if (overcast){
                range 	<- overcast.data$loc_avg
                results <- overcast.data
            } else {
                range 	<- clear_sky.data$loc_avg
                results <- clear_sky.data
            }
            exp_reg <- exp.regression(results,
									  nan.out = nan.out,
									  data_indx = mean.out)
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
        logg("PASS", "Analytical Plots")
    }

    return(list(poster1(),
                poster2(overcast, iter, nan.out, mean.out)))
}

sensor.chart <- function(...){
    #' :detail: overcast distribution charts

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
        logg("PASS", sprintf("Overcast Condition Percentage: %s", gsub("_", " ",snsr_name[count])))
    }
}

sensor.time <- function(overcast){
        #' :detail: Instrumentation time series plots

        # X axis limits
        for (count in col_snsr){
            par(mar=c(3,4, 3, 1), oma=c(1,1,0,0), xpd=FALSE)
            layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
            if(overcast){
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
            # print(length(unique(snsr_tag)[!is.na(unique(snsr_tag))][count]))
            logg("PASS", sprintf("Sky-Ground Time Series: %s", gsub("_", " ",unique(snsr_tag)[count][1])))
        }
    }

data.gen <- function(overcast, dir){
        #' :detail: creates a datafile containing the date, avg temp, and avg pwv for a defined condition
        #' :param bool overcast: the condition of the data (clear sky/overcast)
        #' :param dir: directory path

       ifelse(overcast, res <- overcast.data,
                        res <- clear_sky.data)
      # Pulls the data
        avg_temp  <- res$snsr_sky_calc
        avg_pw 	  <- res$avg
        date      <- res$date
        class(date) <- "Date"
      # Pulls the data
        norm  	  <- data.frame(list(x=date, y1=avg_temp, y2=avg_pw))
      # Removes the NaN data
        norm 	  <- norm[-c(which(avg_pw %in% NaN)), ]
        norm 	  <- norm[-c(which(avg_temp %in% NaN)), ]
      # Adds data to a data frame with column names
        data 	  <- data.frame(list(date=c(norm$x), avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
        colnames(data) <- c("date", "avg_temp", "avg_pw")
      # Writes the data to a csv
        write.csv(data, file=paste(dir, sprintf("data%s.csv", ifelse(overcast,"_overcast", "")), sep=""), row.names=FALSE)
        logg("PASS", sprintf("Data sent to data/data%s.csv", ifelse(overcast,"_overcast", "")))
}

data.ml <- function(dir){
      #' :detail: creates a datafile containing the machine learning relavant information
      #' :param dir: directory path

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
        write.csv(data, file=sprintf("%sml_data.csv", dir), row.names=FALSE)
        logg("PASS", sprintf("Data sent to %sml_data.csv", dir))
}

data.step <- function(seed, i, coef, r, S){
    yml.out <- list(step=c(i),
                    seed=c(seed),
                    analysis=list(coeff=list(A=c(coef$A),
                                             B=c(coef$B)),
                                  rmse=c(r),
                                  rstd=c(S)))
    return(list(yml.out))
}

data.final <- function(dir, clear.len, over.len, train.len, nan.len, frac.kept, coef, std, rmse){
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
        file = paste(dir,"_results.yml", sep=""))
}

visual.products <- function(set, nan.out, mean.out, datetime=datetime, overcast=args$overcast){
    #' :detail: saves plot sets
    #' :param character set: the set identifier
    #' :param logical overcast: ovecast boolean

	if(set == "i"){
        logg("INFO", "Sensor Plot Set")
		pdf(sprintf("%ssensor%s.pdf", fig.dir, ifelse(overcast,"_overcast", "")))
	    sensor.chart()
        sensor.time(overcast)
        return(NULL)
	}else if(set == "t"){
        logg("INFO", "Time Series Plot Set")
		pdf(sprintf("%stime_series%s.pdf", fig.dir, ifelse(overcast,"_overcast", "")))
        if (length(datetime) > 0){
          r1 <- list(res$snsr_sky,
                     res$snsr_gro,
                     res$snsr_del,
                     res$pw_loc,
                     res$tmp_avg,
                     res$loc_avg,
                     list(res$avg),
                     list(res$dew))

          t1 <- list("Sky Temperature Time Series",
                     "Ground Temperature Time Series",
                     "Difference Between Ground-Sky Temperature Time Series",
                     "Total Precipitable Water Time Series",
                     "Temporal Mean Precipitable Water Time Series",
                     "Locational Mean Precipitable Water Time Series",
                     "Mean Total Precipitable Water Time Series",
                     "Dewpoint Temperature Time Series")

            c1 <- unlist(list(rep(list(snsr_color), 3),
                              rep(list(pw_color), 3), "blue", "black"),
                         recursive=FALSE)
            l1 <- unlist(list(rep(list(snsr_name), 3),
                              list(pw_name),
                              list(unique(pw_place)),
                              list(paste(unique(pw_time), "Z")), NA, NA),
                         recursive=FALSE)
            y1 <- unlist(list(rep(list("Temperature [C]"), 3),
                              rep(list("TPW [mm]"), 4), "Temperature [C]"),
                         recursive=FALSE)
            par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
            for (i in 1:length(r1)){
              time.nth_range(r1[[i]], t1[[i]], c1[[i]], l1[[i]], y1[[i]], datetime, overcast)
            }
            r2 <- list(list(res$snsr_sky_calc, res$avg),
                       list(res$snsr_sky_calc, res$rh),
                       list(res$avg, res$rh))
            t2 <- list("Mean Sky Temperature and PWV Time Series",
                        "Mean Sky Temperature and RH Time Series",
                       "Mean TPW and RH Time Series")
            c2 <- list(list("red", "blue"),
                       list("red", "green3"),
                       list("blue", "green3"))
            y2 <- list(list("Temperature [C}", "TPW [mm]"),
                       list("Temperature [C]", "RH [%]"),
                       list("TPW [mm]", "RH [%]"))
            par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
            for (i in 1:length(r2)){
              time.composite(r2[[i]], t2[[i]], c2[[i]], y2[[i]], datetime, overcast)
            }
            time.pwindex(datetime)
            return(NULL)
		} else {
			logg("ERROR", D01); closing()
		}
	}else if(set == "a"){
        logg("INFO", "Analytics Plot Set")
        pdf(sprintf("%sanalytics%s.pdf", fig.dir,
                             ifelse(overcast,"_overcast", "")))
        ifelse(overcast, res <- overcast.data,
                         res <- clear_sky.data)

        x1 <- unlist(list(rep(list(res$snsr_sky_calc), 3)),
                     recursive=FALSE)
        y1 <- list(res$pw_loc, res$loc_avg, res$tmp_avg)
        t1 <- list("Correlation between TPW and Temperature",
                   "Correlation between Locational Mean TPW and Temperature",
                   "Correlation between Temporal Mean TPW and Temperature")
        l1 <- rep(list(list("Zenith Sky Temperature [C]", "TPW [mm]")), 3)
        c1 <- unlist(list(rep(list(pw_color), 3)), recursive=FALSE)
        leg.lab <- unlist(list(list(pw_name),
                               list(unique(pw_place)),
                               list(paste(unique(pw_time), "Z"))),
                          recursive=FALSE)

        for (i in 1:length(x1)){
          analysis.nth_range(overcast, x1[[i]], y1[[i]], t1[[i]], l1[[i]], c1[[i]], leg.lab[[i]])
        }

        x2 <- unlist(list(rep(list(res$snsr_sky_calc), 2)),
                     recursive=FALSE)
        y2 <- list(res$avg, res$wt_avg)
        t2 <- list("Regression between Mean TPW and Temperature",
                   "Regression between Weighted TPW and Temperature")
        l2 <- rep(list(list("Zenith Sky Temperature [C]", "TPW [mm]")), 2)
        for (i in 1:length(x2)){
          analysis.regression(overcast, x2[[i]], y2[[i]], t2[[i]], l2[[i]], iter.results, res)
        }

        ml.x <- c(clear_sky.data$snsr_sky_calc, overcast.data$snsr_sky_calc)
        ml.y <- c(clear_sky.data$wt_avg, overcast.data$wt_avg)
        ml.l <- c(clear_sky.data$label, overcast.data$label)
        nan.ml <- nan.filter(list(x=ml.x, y=ml.y, l=ml.l))[[1]]
        ml <- lsvm(nan.ml$x, log(nan.ml$y, base=exp(1)), nan.ml$l)
        analysis.svm(ml)
	}else if(set == "c"){
        logg("INFO", "Chart Set")
		pdf(sprintf("%scharts.pdf", fig.dir))
        charts()
        return(NULL)
	} else if (set == "p") {
        logg("INFO", "Pac-Man Plot Set")
		pdf(sprintf("%spacman%s.pdf", fig.dir, ifelse(overcast,"_overcast", "")))
        ifelse(overcast, results <- overcast.data,
                         results <- clear_sky.data)
        exp.reg <-  exp.regression(results,train_frac,
                               nan.out = nan.out,
                               data_indx = mean.out)
        x1 <- list(exp.reg$x)
        y1 <- list(exp.reg$y)
        t1 <- list("Correlation between Mean TPW and Temperature")
        th1 <- list(c("Zenith Sky Temperature", "C"))
        ra1 <- list(c("TPW", "mm"))
        for (i in 1:length(x1)){
          pac.compare(overcast, t1[[i]], x1[[i]], y1[[i]], th1[[i]], ra1[[i]])
        }
        pac.regression(overcast)
        return(NULL)
	} else if (set == "o"){
        logg("INFO", "Poster Plot Set")
		pdf(sprintf("%sposter%s.pdf", fig.dir, ifelse(overcast,"_overcast", "")))
        poster.plots(overcast, iter.results, nan.out, mean.out)
        return(NULL)
	}
}