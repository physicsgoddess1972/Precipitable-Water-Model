#' :file: pmat_products.r
#' :module: Precipitable Water Model Analysis Tool: Products
#' :synopsis: plotting functions for PMAT
#' :author: Spencer Riley <sriley@pmat.app>

time_series.plots <- function(datetime, overcast){
    #' :details: The set of all time series plots
    #' :param date: the datestamp of the data
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :return: All available time series plots

    par(mar = c(3,4, 3, 5.5), oma = c(1, 1, 0, 0), mfrow=c(1, 1), xpd=TRUE)

    time9 	<- function(){
        #' :details: Sky Temperature - RH Time Series
        #' :param date: the datestamp of the data
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        par(mar = c(3,4, 3, 3), oma = c(1, 1, 0, 0), mfrow=c(1, 1), xpd=TRUE)
        # Plotting margin
        range_over 		<- overcast.results$pw.index
        title 		    <- sprintf("Normalized Index for Weighted PWV Time Series")
        range_clear	    <- clear_sky.results$pw.index

        datetime_clear <- as.POSIXct(paste(as.Date(unlist(clear_sky.results$date), origin="1970-01-01"),
                                           paste(unlist(clear_sky.results$time),":00", sep="")),
                                     format="%Y-%m-%d %H:%M:%S")
        datetime_over <- as.POSIXct(paste(as.Date(unlist(overcast.results$date), origin="1970-01-01"),
                                           paste(unlist(overcast.results$time),":00", sep="")),
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

    time.nth_range <- function(range, title, color, leg.lab, ylab){
        #' :details: Sky Temperature - RH Time Series
        #' :param date: the datestamp of the data
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        for (i in 1:length(range)){
            test <- unlist(range[[i]])
            test[!is.finite(unlist(range[[i]]))] <- NA
            # defines the max and min of the y-axis
            ymin <- min(as.numeric(unlist(test)), na.rm=TRUE)
            ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
            plot(datetime, range[[i]][[1]],
                 ylab=ylab[[i]][1],
                 xaxt='n',
                 main=stnd_title(title[[i]], overcast),
                 pch=16,
                 ylim=c(ymin, ymax),
                 xlim=time_axis_init(datetime)[[1]],
                 col=color[[i]][1])

            time_axis(datetime)
            # plots all other ranges in the list if the length of range is greater than 1
            if (length(range[[i]]) > 1){
                for(j in 2:length(range[[i]])){
                    points(datetime, range[[i]][[j]], pch=16, col=color[[i]][j])
                }
            }
            # defines the legend
            if (!is.na(leg.lab[[i]])){
                legend("topright", inset=c(-0.21, 0),
                                   legend=c(gsub("_", " ",leg.lab[[i]])),
                                   col=c(color[[i]]),
                                   pch=c(16,16, 16))
            }
            # print statement when completed
            logg("PASS", title[[i]])
        }
    }

    time.composite <- function(range, title, color, ylab){
        #' :details: Sky Temperature - RH Time Series
        #' :param date: the datestamp of the data
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        for (i in 1:length(range)){
            plot(datetime, range[[i]][[1]],
                 ylab=NA,
                 col=color[[i]][[1]],
                 main=stnd_title(title[[i]], overcast),
                 xaxt='n',
                 pch=16,
                 xlim=time_axis_init(datetime)[[1]])

            axis(side = 2); mtext(side = 2, line=3, ylab[[i]][[1]], col=color[[i]][[1]])

            time_axis(datetime)

            par(new = T)
            plot(datetime, range[[i]][[2]],
                 ylab=NA,
                 axes=F,
                 xlab=NA,
                 col=color[[i]][[2]],
                 pch=16,
                 xlim=time_axis_init(datetime)[[1]])
            axis(side = 4); mtext(side=4, line=3, ylab[[i]][[2]], col=color[[i]][[2]])
            logg("PASS", title[[i]])
        }
    }

    ifelse(overcast, res <- overcast.results,
                     res <- clear_sky.results)

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

    r2 <- list(list(res$snsr_sky_calc, res$rh),
               list(res$avg, res$rh))
    t2 <- list("Mean Sky Temperature and RH Time Series",
               "Mean TPW and RH Time Series")
    c2 <- list(list("red", "green3"),
               list("blue", "green3"))
    y2 <- list(list("Temperature [C]", "RH [%]"),
               list("TPW [mm]", "RH [%]"))

    return(list("time.nth"=time.nth_range(r1, t1, c1, l1, y1),
                "time.composite"=time.composite(r2, t2, c2, y2),
                "time9"=time9()))
}

analytical.plots <- function(overcast, iter){
    #' :details: The set of all analytical plots
    #' :param bool overcast: the condition of data (clear sky/overcast)
    #' :return: All available analytical plots
    par(mar = c(5.1,4, 4, 5.5), oma = c(1, 1, 0, 0), mfrow=c(1, 1), xpd=TRUE)

    analysis.nth_range <- function(overcast, x, y, title, label, color, leg.lab){
        #' :details: Super Average Plot with Exponential Fit
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        for (i in 1:length(x)){
            ymax	<- max(unlist(y[[i]]), na.rm=TRUE)
            ymin	<- min(unlist(y[[i]]), na.rm=TRUE)
            col <- colscheme(y[[i]])

            plot(x[[i]],  y[[i]][[1]],
                 xlab=label[[i]][[1]],
                 ylab=label[[i]][[2]],
                 ylim=c(ymin, ymax),
                 main=stnd_title(title[[i]], overcast),
                 pch=16,
                 col=color[[i]][[1]])
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            if (length(y[[i]]) >= 2){
                for(j in 2:length(y[[i]])){
                    points(x[[i]], y[[i]][[j]], pch=16, col=color[[i]][[j]])
                }
            }
            if (!is.na(leg.lab[[i]])){
                legend("topright",  inset=c(-0.21, 0),
                                    legend=leg.lab[[i]],
                                    col=color[[i]],
                                    pch=c(16,16, 16))
            }
            logg("PASS", title[[i]])
        }
    }

    plots4 	<- function(overcast, iter){
        #' :details: Super Average Plot with Exponential Fit
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)

        ifelse(overcast, results <- overcast.results,
                         results <- clear_sky.results)
        x <- results$snsr_sky_calc
        y <- results$avg
        des     <- "Correlation between Mean TPW and Temperature"

        xmin <- min(x, na.rm=TRUE); xmax <- max(x, na.rm=TRUE)
        newx <- seq(xmin, xmax, length.out=length(x))

        ymax <- max(y, na.rm=TRUE); ymin <- min(y, na.rm=TRUE)
        # Non-linear model (exponential)
        exp_reg <- exp.regression(results, 1)

        plot(exp_reg$x,exp_reg$y,
             pch=1,
             ylim=c(ymin, ymax),
             xlab="Zenith Sky Temperature [C]",
             ylab="TPW [mm]",
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

    plots5 	<- function(overcast, iter){
        #' :details: Individual PW Location plots
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)
        ifelse(overcast, results <- overcast.results,
                         results <- clear_sky.results)
        x <- results$snsr_sky_calc
        y <- results$wt_avg
        des     <- "Correlation between Weighted Mean TPW and Temperature"

        xmin <- min(x, na.rm=TRUE); xmax <- max(x, na.rm=TRUE)
        newx <- seq(xmin, xmax, length.out=length(x))
        ymax <- max(y, na.rm=TRUE); ymin <- min(y, na.rm=TRUE)
        # Non-linear model (exponential)
	   	exp_reg <- exp.regression(results, 1)

        plot(exp_reg$x,exp_reg$y,
             pch=1,
             ylim=c(ymin, ymax),
             xlab="Zenith Sky Temperature [C]",
             ylab="TPW [mm]",
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

    ifelse(overcast, res <- overcast.results,
                     res <- clear_sky.results)

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

    return(list(analysis.nth_range(overcast, x1, y1, t1, l1, c1, leg.lab),
                plots4(overcast, iter),
                plots5(overcast, iter)))
}

pac.plots <- function(overcast){
#' :details: Pac-Man plot set
#' :param bool overcast: the condition of data (clear sky / overcast)
#' :return: All available Pac-Man plots

    pac1 <- function(overcast){
        #' :details: Pac-Man plot of Super Average Plot
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)
        ifelse(overcast, results <- overcast.results,
                         results <- clear_sky.results)
        des <- "Correlation between Mean TPW and Temperature"

       	exp_reg <- exp.regression(results, 1)
        # Finds and removes NaNed values from the dataset
        pac.plot(exp_reg$x,exp_reg$y,
                 stnd_title(des, overcast),
                 c("Zenith Sky Temperature", "C"),
                 c("TPW", "mm"))

       logg("PASS", "Total Mean PW and Temperature")
    }

    pac2 <- function(overcast){
        #' :details: Pac-Man residual plot
        #' :param bool overcast: the condition of data (clear sky/overcast)
        #' :return: A sky temperature time series plot

        ifelse(overcast, x <- overcast.results$snsr_sky_calc,
                         x <- clear_sky.results$snsr_sky_calc)
        ifelse(overcast, y <- log(overcast.results$avg, base=exp(1)),
                         y <- log(clear_sky.results$avg, base=exp(1)))
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

    return(list(pac1(overcast),
                pac2(overcast)))
}

charts	<- function(...){
    #' :details: A collection of histograms and charts
    #' :return: PDF of charts

    chart1 <- function(range, xlabel, title){
        #' :details: Histograms of defined quantities
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
    x <- list(c(cbind(overcast.results$wt_avg, clear_sky.results$wt_avg)),
              cbind(unlist(overcast.results$snsr_sky_calc),
                     unlist(clear_sky.results$snsr_sky_calc)),
              cbind(overcast.results$rh, clear_sky.results$rh),
              cbind(overcast.results$dew, clear_sky.results$dew))

    xlab <- list("Precipitable Water [mm]",
                 "Temperature [C]",
                "Relative Humidity [%]",
                "Temperature [C]")
    title <- list("Weighted PWV",
                  "Average Temperature",
                  "Relative Humidity",
                  "Dewpoint Temperature")

    for (i in 1:length(unique(pw_place))){
        x <- append(x, list(c(cbind(unlist(clear_sky.results$tmp_avg[i]),
                                    unlist(overcast.results$tmp_avg[i])))))
        xlab <- append(xlab, "Precipitable Water [mm]")
        title <- append(title, sprintf("Distribution of PWV in %s", unique(pw_place)[i]))
    }
    return(list(chart1(x, xlab, title)))
}

poster.plots <- function(overcast, iter){
    #' :details: The set of all poster
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
        xmin <- min(do.call("c", clear_sky.results$date), na.rm=TRUE)
        xmax <- max(do.call("c", clear_sky.results$date), na.rm=TRUE)

        ymax <- max(c(as.numeric(unlist(overcast.results$snsr_sky)),
                      as.numeric(unlist(clear_sky.results$snsr_sky))), na.rm=TRUE)
        ymin <- min(c(as.numeric(unlist(overcast.results$snsr_sky)),
                      as.numeric(unlist(clear_sky.results$snsr_sky))), na.rm=TRUE)

        range_index <- clear_sky.results$snsr_sky
        clear_date <- clear_sky.results$date
        over_date <- overcast.results$date

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

        range_index <- overcast.results$snsr_sky

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
        ymax  		<- max(c(as.numeric(unlist(clear_sky.results$snsr_gro)),
                             as.numeric(unlist(clear_sky.results$snsr_gro))),na.rm=TRUE)
        ymin  		<- min(c(as.numeric(unlist(clear_sky.results$snsr_gro)),
                             as.numeric(unlist(overcast.results$snsr_gro))),na.rm=TRUE)
        range_index <- clear_sky.results$snsr_gro

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
        range_index <- overcast.results$snsr_gro

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
        ymax 		<- max(c(as.numeric(unlist(clear_sky.results$snsr_del)),
                             as.numeric(unlist(overcast.results$snsr_del))), na.rm=TRUE)
        ymin 		<- min(c(as.numeric(unlist(clear_sky.results$snsr_del)),
                             as.numeric(unlist(overcast.results$snsr_del))), na.rm=TRUE)
        range_index <- clear_sky.results$snsr_del

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
        range_index <- overcast.results$snsr_del

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

    poster2 <- function(overcast, iter){
        #' :details: The analytics poster plot
        #' :param bool overcast: the condition of data (clear sky/overcast)

        ## Layout/Margin Configuration
            par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
            layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
        ## Locational Averagen PW Temperature Correlation
            if (overcast){
                xmax 	<- max(as.numeric(unlist(overcast.results$snsr_sky_calc)), na.rm=TRUE)
                xmin 	<- min(as.numeric(unlist(overcast.results$snsr_sky_calc)), na.rm=TRUE)
                x 		<- as.numeric(unlist(overcast.results$snsr_sky_calc))

                range 	<- overcast.results$tmp_avg
            } else {
                xmax 	<- max(as.numeric(unlist(clear_sky.results$snsr_sky_calc)), na.rm=TRUE)
                xmin 	<- min(as.numeric(unlist(clear_sky.results$snsr_sky_calc)), na.rm=TRUE)
                x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))

                range 	<- clear_sky.results$tmp_avg
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
            legend("topleft", legend=unique(pw_time), col=colscheme(range), pch=c(16))

        ## Temporal Average Pw Temperature Correlation
            if (overcast){
                range 	<- overcast.results$loc_avg
                results <- overcast.results
            } else {
                range 	<- clear_sky.results$loc_avg
                results <- clear_sky.results
            }
            exp_reg <- exp.regression(results, 1)
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
                poster2(overcast, iter)))
}

instr 	<- function(overcast){
    #' :details: collection of figures based on individual sensor data
    #' :param bool overcast: the condition of the data (clear sky/overcast)
    #' :return: Instrumentation time series plots and overcast distribution charts

    chart <- function(...){
        #' :details: overcast distribution charts

        par(mar=c(5.1, 5.1, 5.1, 1.3),oma=c(0,0,0,0), xpd=TRUE)
        snsr_sky  <- clear_sky.results$raw_sky
        snsr_skyo <- overcast.results$raw_sky

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

    time <- function(...){
        #' :details: Instrumentation time series plots

        # X axis limits
        for (count in col_snsr){
            par(mar=c(3,4, 3, 1), oma=c(1,1,0,0), xpd=FALSE)
            layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
            if(overcast){
                date 		<- overcast.results$date
                time        <- overcast.results$time
                snsr_sky    <- overcast.results$snsr_sky
                snsr_gro    <- overcast.results$snsr_gro
                sky_title 	<- sprintf("Condition: Overcast \n Sky Temperature Time Series for %s", snsr_tag[count[1]])
                gro_title 	<- sprintf("Ground Temperature Time Series for %s", snsr_tag[count[1]])
            }else{
                date 		<- clear_sky.results$date
                time        <- clear_sky.results$time
                snsr_sky    <- clear_sky.results$snsr_sky
                snsr_gro    <- clear_sky.results$snsr_gro
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
    return(list(chart(), time()))
}

data.products <- function(overcast, dir, i){
    #' :details: A collection of datafiles
    #' :return: datafiles

    data1 <- function(overcast, dir){
        #' :details: creates a datafile containing the date, avg temp, and avg pwv for a defined condition
        #' :param bool overcast: the condition of the data (clear sky/overcast)
        #' :param dir: directory path

        if (overcast){
        # Pulls the data
          avg_temp	<- as.numeric(unlist(overcast.results$snsr_sky_calc))
          avg_pw 	<- overcast.results$avg
          date      <- overcast.results$date
          class(date) <- "Date"
        # Pulls the data
          norm  		<- data.frame(list(x=date, y1=avg_temp, y2=avg_pw))
        # Removes the NaN data
          norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
          norm 		<- norm[-c(which(avg_temp %in% NaN)), ]
        # Adds data to a data frame with column names
          data 		<- data.frame(list(date=c(norm$x), avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
          colnames(data) <- c("date", "avg_temp", "avg_pw")
        # Writes the data to a csv
          write.csv(data, file=sprintf("../data/data_overcast.csv"), row.names=FALSE)
          logg("PASS", sprintf("Data sent to data/data_overcast.csv"))
        }else{
        # Pulls the data
            avg_temp	<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            avg_pw 		<- clear_sky.results$avg
            date        <- unlist(clear_sky.results$date)
            class(date) <- "Date"
        # Pulls the data
            norm  		<- data.frame(list(x=date, y1=avg_temp, y2=avg_pw))
        # Removes the NaN data
            norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
            norm 		<- norm[-c(which(avg_temp %in% NaN)), ]
           data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
            colnames(data) <- c("date", "avg_temp", "avg_pw")
        # Writes the data to a csv
            write.csv(data, file=sprintf("%sdata_clear.csv", dir), row.names=FALSE)
            logg("PASS", sprintf("Data sent to %sdata_clear.csv", dir))

        }
}

    data2 <- function(dir){
      #' :details: creates a datafile containing the machine learning relavant information
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
        date 		<- as.Date(fname[ ,col_date], "%m/%d/%Y")
        cond 		<- fname[,col_con]
        # Pulls the data
        norm  		<- na.omit(data.frame(list(x=date, y1=avg_temp, y2=avg_pw, y3=avg_rh, c=cond)))
        data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2), avg_rh=c(norm$y3), cond=c(norm$c)))
        colnames(data) <- c("date", "avg_temp", "avg_pw", "avg_rh", "condition")
        # Writes the data to a csv
        write.csv(data, file=sprintf("%sml_data.csv", dir), row.names=FALSE)
        logg("PASS", sprintf("Data sent to %sml_data.csv", dir))
        }
    if (i == 'a'){
        return(data1(overcast, dir))
    } else if (i == 'm'){
        return(data2(dir))
    }
}

visual.products <- function(set, overcast){
    #' :details: saves plot sets
    #' :param character set: the set identifier
    #' :param logical overcast: ovecast boolean
    if(set == "a" || set == "o"){
        ifelse(overcast, len <- length(overcast.results$date),
                         len <- length(clear_sky.results$date))
        if (len > 0){
            iter.results <- iterative.analysis(overcast, args$dir, args$u)
        } else {
            logg("ERROR", D01); closing()
        }
    }

	if(set == "i"){
        logg("INFO", "Sensor Plot Set")
		sname_pub <- sprintf("%ssensor_%s.pdf", fig_dir, ifelse(overcast,"overcast", ""))
		save(c(instr(overcast)), sname_pub)
        return(NULL)
	}else if(set == "t"){
        logg("INFO", "Time Series Plot Set")
		ifelse(overcast, 	date <- overcast.results$date,
								date <- clear_sky.results$date)
		ifelse(overcast, 	time <- overcast.results$time,
								time <- clear_sky.results$time)

		datetime <- as.POSIXct(paste(as.Date(unlist(date), origin="1970-01-01"),
									paste(unlist(time),":00", sep="")),
							   format="%Y-%m-%d %H:%M:%S")

		sname_pub <- sprintf("%stime_series_%s.pdf", fig_dir, ifelse(overcast,"overcast", ""))

		if (length(date) > 0){
			save(c(time_series.plots(datetime, overcast)), sname_pub)
            return(NULL)
		} else {
			logg("ERROR", D01); closing()
		}
	}else if(set == "a"){
        logg("INFO", "Analytics Plot Set")
		sname_pub <- sprintf("%sanalytics_%s.pdf", fig_dir, ifelse(overcast,"overcast", ""))
		save(c(analytical.plots(overcast, iter.results)), sname_pub)
        return(NULL)
	}else if(set == "c"){
        logg("INFO", "Chart Set")
		sname_pub 	<-sprintf("%scharts.pdf", fig_dir)
		save(c(charts()), sname_pub)
        return(NULL)
	} else if (set == "p") {
        logg("INFO", "Pac-Man Plot Set")
		sname_pub <- sprintf("%spacman_%s.pdf", fig_dir, ifelse(overcast,"overcast", ""))
		save(c(pac.plots(overcast)), sname_pub)
        return(NULL)
	} else if (set == "o"){
        logg("INFO", "Poster Plot Set")
		sname_pub <- sprintf("%sposter_%s.pdf", fig_dir, ifelse(overcast,"overcast", ""))
		save(c(poster.plots(overcast, iter.results)), sname_pub)
        return(NULL)
	}
}