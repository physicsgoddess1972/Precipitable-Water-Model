#' @file pmat_plots.r
#' @author Spencer Riley
#' @brief functions for all plots
#' @docs https://docs.pmat.app
#' @help To get a list of arguments run [Rscript model.r --help]

#' @title time_series.plot
#' @description The set of all time series plots
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return All available time series plots
#' @export
time_series.plots <- function(date, overcast){
    force(date); force(overcast)

#' @title time1
#' @description Sky Temperature plot
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
    time1 	<- function(date, overcast){
        # Plotting margin
        if(overcast){
            range 		    <- overcast.results$snsr_sky
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Overcast")
        }else{
            range	 		<- clear_sky.results$snsr_sky
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Clear Sky")
        }
        # removes all data that is not defined
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA
        # defines the max and min of the y-axis
        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        # plots the first range in the list
        plot(date, t(unlist(range[1])),
             xlab="Date",
             ylab="Temperature [C]",
             xaxt='n',
             main=title,
             pch=16,
             ylim=c(ymin, ymax),
             col=snsr_color[1])

        # defines major and minor tick marks for the x-axis and their position
        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        # plots all other ranges in the list if the length of range is greater than 1
        if (length(range) > 1){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=snsr_color[i])
            }
        }
        # defines the legend
        legend("topright", inset=c(-0.21, 0),
                           legend=c(gsub("_", " ",snsr_name)),
                           col=c(snsr_color),
                           pch=c(16,16, 16))
        # print statement when completed
        cat(green("[1]"), "Sky Temperature Time Series\n")
    }

#' @title time2
#' @description Ground Temperature plot
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time2 	<- function(date, overcast){
        if(overcast){
            range 		<- overcast.results$snsr_gro
            title 		<- "Ground Temperature Time Series \n Condition: Overcast"
        }else{
            range		<- clear_sky.results$snsr_gro
            title 		<- "Ground Temperature Time Series \n Condition: Clear Sky"
        }
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA

        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)

        # Legend configuration
        plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
            main=title, pch=16, ylim=c(ymin, ymax), col=snsr_color[1])

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=snsr_color[i])
            }
        }
        legend("topright", inset=c(-0.21, 0), legend=c(gsub("_", " ",snsr_name)), col=c(snsr_color), pch=c(16,16, 16))
        cat(green("[2]"), "Ground Temperature Time Series\n")
    }

#' @title time3
#' @description Change in temperature plot
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time3 	<- function(date, overcast){
        if(overcast){
            range 		<- overcast.results$snsr_del
            title 		<- sprintf("Difference Between Ground-Sky Temperature Time Series \n Condition: Overcast")
        }else{
            range 		<- clear_sky.results$snsr_del
            title 		<- sprintf("Difference Between Ground-Sky Temperature Time Series \n Condition: Clear Sky")
        }
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA

        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
        main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[unique(seq(1, length(ticks.at), length.out=5))]
        mn_ticks <- ticks.at[unique(-(seq(1, length(ticks.at), length.out=5)))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=snsr_color[i])
            }
        }
        legend("topright", inset=c(-0.21, 0), legend=c(gsub("_", " ",snsr_name)), col=c(snsr_color), pch=c(16,16, 16))
        cat(green("[3]"), "Change in Temperature between Sky and Ground Time Series\n")
    }

#' @title time4
#' @description PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time4		<- function(date, overcast){
        if(overcast){
            range 		<- overcast.results$pw_loc
            title 		<- "Total Precipitable Water Time Series \n Condition: Overcast"
        }else{
            range 		<- clear_sky.results$pw_loc
            title 		<- "Total Precipitable Water Time Series \n Condition: Clear Sky"
        }
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA

        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date, t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        legend("topright", inset=c(-0.205, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16), cex=0.95)
        cat(green("[4]"), "Precipitable Water Time Series\n")
    }

#' @title time5
#' @description Sky temperature - PW composite time series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @export
    time5 	<- function(date, overcast){
        if(overcast){
            range1 	<- as.numeric(unlist(overcast.results$snsr_sky_calc))
            range2 	<- overcast.results$avg
            title 	<- sprintf("Mean Sky Temperature and TPW Time Series \n Condition: Overcast");
        }else{
            range1 	<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            range2 	<- clear_sky.results$avg
            title 	<- sprintf("Mean Sky Temperature and TPW Time Series \n Condition: Clear Sky")
        }
        plot(date, range1, ylab=NA, xlab="Date", col="red", pch=16, main=title, xaxt='n')

        ticks.at <- seq(date[[1]], date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")
        par(new = T)
        plot(date, range2, ylab=NA, axes=F, xlab=NA, col="blue", pch=16)
        axis(side = 4); mtext(side = 4, line=3, "TPW [mm]", col="blue")
        cat(green("[5]"), "Sky Temperature - Precipitable Water Time Series\n")
    }

#' @title time6
#' @description Temporal Mean PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @export
    time6 	<- function(date, overcast){
        if(overcast){
            range 		<- overcast.results$tmp_avg
            title 		<- "Temporal Average TPW Time Series \n Condition: Overcast"
        }else{
            range 		<- clear_sky.results$tmp_avg
            title 		<- "Temporal Average TPW Time Series \n Condition: Clear Sky"
        }
        ymin <- min(as.numeric(unlist(range)), na.rm=TRUE); ymax <- max(as.numeric(unlist(range)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date,  t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])

        ticks.at <- seq(date[[1]], date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        if (length(range) >= 2){
            for(i in 1:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        legend("topright", inset=c(-0.153, 0), legend=c(unique(pw_place)), col=c(pw_color), pch=c(16,16, 16))
        cat(green("[6]"), "Temporal Mean Precipitable Water Time Series\n")
    }

#' @title time7
#' @description Locational Mean PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time7 	<- function(date, overcast){
        if(overcast){
            range 	<- overcast.results$loc_avg
            title 	<- "Locational Average TPW Time Series \n Condition: Overcast"
        }else{
            range 	<- clear_sky.results$loc_avg
            title 	<- "Locational Average TPW Time Series \n Condition: Clear Sky"
        }
        ymin <- min(as.numeric(unlist(range)), na.rm=TRUE); ymax <- max(as.numeric(unlist(range)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date,  t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        legend("topright", inset=c(-0.14, 0), legend=c(unique(pw_time)), col=pw_color, pch=c(16,16))
        cat(green("[7]"), "Locational Mean Precipitable Water Time Series\n")
    }

#' @title time8
#' @description Mean PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time8 	<- function(date, overcast){
        if(overcast){
            range 		<- overcast.results$avg
            title 		<- "Mean TPW Time Series \n Condition: Overcast"
        }else{
            range 		<- clear_sky.results$avg
            title 		<- "Mean TPW Time Series \n Condition: Clear Sky"
        }
        ymin <- min(as.numeric(unlist(range)), na.rm=TRUE); ymax <- max(as.numeric(unlist(range)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date,  t(unlist(range)),   xlab="Date",
                                        ylab="TPW [mm]",
                                        xaxt='n',
                                        xlim=c(xmin, xmax),
                                        ylim=c(ymin, ymax),
                                        main=title,
                                        pch=16,
                                        col="blue")
        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        cat(green("[8]"), "Mean Precipitable Water Time Series\n")
    }

#' @title time9
#' @description PW - RH compostie time series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time9 	<- function(date, overcast){
        if(overcast){
            range1 	<- overcast.results$avg
            range2  <- overcast.results$rh
            title 	<- sprintf("Mean TPW and RH Time Series \n Condition: Overcast");
        }else{
            range1 	<- clear_sky.results$avg
            range2 	<- clear_sky.results$rh
            title 	<- sprintf("Mean TPW and RH Time Series \n Condition: Clear Sky")
        }

        plot(date, range1, ylab=NA, xlab="Date", col="blue", main=title, xaxt='n', pch=16)
        axis(side = 2); mtext(side = 2, line=3, "TPW [mm]", col="blue")

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        par(new = T)
        plot(date, range2, ylab=NA, axes=F, xlab=NA, col="green3", pch=16)
        axis(side = 4); mtext(side=4, line=3, "RH [%]", col="green3")
    	cat(green("[9]"), "Precipitable Water - RH Time Series\n")
    }

#' @title time10
#' @description Sky Temperature - RH Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time10 	<- function(date, overcast){
        if(overcast){
            range1 	<- as.numeric(unlist(overcast.results$snsr_sky_calc))
            range2  <- overcast.results$rh
            title 	<- sprintf("Mean Sky Temperature and RH Time Series \n Condition: Overcast");
        }else{
            range1 	<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            range2 	<- clear_sky.results$rh
            title 	<- sprintf("Mean Sky Temperature and RH Time Series \n Condition: Clear Sky")
        }

        plot(date, range1, ylab=NA, xlab="Date", col="red", main=title, xaxt='n', pch=16)
        axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        par(new = T)
        plot(date, range2, ylab=NA, axes=F, xlab=NA, col="green3", pch=16)
        axis(side = 4); mtext(side=4, line=3, "RH [%]", col="green3")
    	cat(green("[10]"), "Sky Temperature - RH Time Series\n")
    }

    return(list(time1(date, overcast),
                time2(date, overcast),
                time3(date, overcast),
                time4(date, overcast),
                time5(date, overcast),
                time6(date, overcast),
                time7(date, overcast),
                time8(date, overcast),
                time9(date, overcast),
                time10(date, overcast)))
}

#' @title analytical.plots
#' @description The set of all analytical plots
#' @param overcast the condition of data (clear sky/overcast)
#' @return All available analytical plots
#' @export
analytical.plots <- function(overcast, exp_reg){
#' @title plots1
#' @description Individual PW Location plots
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots1 	<- function(overcast){
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            x 		<- as.numeric(unlist(overcast.results$snsr_sky_calc))
            range 	<- overcast.results$pw_loc
            title 	<- "Correlation between TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            range 	<- clear_sky.results$pw_loc
            title	<- "Correlation between TPW and Temperature \n Condition: Clear Sky"
        }
        ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE); ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)
        plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(x, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        legend("topright", inset=c(-0.205, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16), cex=0.95)
        cat(green("[1]"), "Correlation between PW and Temperature\n")
    }

#' @title plots2
#' @description Locational Average PW plots
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots2 	<- function(overcast){
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            x 		<- as.numeric(unlist(overcast.results$snsr_sky_calc))
            range 	<- overcast.results$loc_avg
            title 	<- "Correlation between Temporal Mean TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            range 	<- clear_sky.results$loc_avg
            title 	<- "Correlation between Temporal Mean TPW and Temperature \n Condition: Clear Sky"
        }
        ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE); ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)
        col <- colscheme(range)

        plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]",ylim=c(ymin, ymax), main=title, pch=16, col=col[1])

        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(x, t(unlist(range[i])), pch=16, col=col[i])
            }
        }
        legend("topright", inset=c(-0.153, 0), legend=c(unique(pw_place)), col=col, pch=c(16,16, 16))
        cat(green("[2]"), "Correlation between Locational Mean PW and Temperature\n")
    }

#' @title plots3
#' @description Temporal Average PW plots
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots3 	<- function(overcast){
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)

        if(overcast){
            x 		<- as.numeric(unlist(overcast.results$snsr_sky_calc))
            range 	<- overcast.results$tmp_avg
            title 	<- "Correlation between Locational Mean TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            range 	<- clear_sky.results$tmp_avg
            title 	<- "Correlation between Locational Mean TPW and Temperature \n Condition: Clear Sky"
        }
        ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE); ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)
        col     <- colscheme(range)

        plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]",
        #xlim=c(xmin, xmax),
        ylim=c(ymin, ymax), main=title, pch=16, col=col[1])
        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(x, t(unlist(range[i])), pch=16, col=col[i])
            }
        }
        legend("topright", inset=c(-0.14, 0), legend=c(unique(pw_time)), col=col, pch=c(16,16, 16))
        cat(green("[3]"), "Correlation between Temporal Mean PW and Temperature\n")
    }
#' @title plots4
#' @description Super Average Plot with Exponential Fit
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots4 	<- function(overcast){
        par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)
        if(overcast){
            x 		<- as.numeric(unlist(overcast.results$snsr_sky_calc))
            y       <- overcast.results$avg
            z       <- overcast.results$pw_loc
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            y       <- clear_sky.results$avg
            z       <- clear_sky.results$pw_loc
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Clear Sky"
        }
        xmin <- min(x, na.rm=TRUE); xmax <- max(x, na.rm=TRUE)
        newx <- seq(xmin, xmax, length.out=length(x))
        ymax <- max(y, na.rm=TRUE); ymin <- min(y, na.rm=TRUE)
        # Non-linear model (exponential)
	    exp_reg <- exp.regression(x, y, z, 1)

        plot(x,y, pch=1, ylim=c(ymin, ymax), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", main=title)
        # Best Fit
        curve(iter.results$A*exp(iter.results$B*x), col="black", add=TRUE)

        polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)
        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        legend("topleft",col=c("black"), lty=c(1),
        legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f*mm)",
        iter.results$A,iter.results$B,iter.results$S))))
        cat(green("[4]"), "Total Mean PW and Temperature\n")
    }

    return(list(plots1(overcast),
                plots2(overcast),
                plots3(overcast),
                plots4(overcast)))
}

#' @title pac.plots
#' @description Pac-Man plot set
#' @param overcast the condition of data (clear sky / overcast)
#' @return All avialable Pac-Man plots
#' @export
pac.plots <- function(overcast){
#' @title pac1
#' @description Pac-Man plot of Super Average Plot
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    pac1 <- function(overcast){
        par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)
        if(overcast){
            x 		<- as.numeric(unlist(overcast.results$snsr_sky_calc))
            y       <- overcast.results$avg
            z       <- overcast.results$pw_loc
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            y       <- clear_sky.results$avg
            z       <- clear_sky.results$pw_loc
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Clear Sky"
        }
        exp_reg <- exp.regression(x,y,z, 1)
        # Finds and removes NaNed values from the dataset
        pac.plot(exp_reg$x,exp_reg$y, title, c("Zenith Sky Temperature", "C"),c("TPW", "mm"))
        cat(green("[1]"), "Total Mean PW and Temperature\n")
    }

#' @title pac2
#' @description Pac-Man residual plot
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    pac2 <- function(overcast){
        if(overcast){
            x <- as.numeric(unlist(overcast.results$snsr_sky_calc))
            y <- log(overcast.results$avg, base=exp(1))
            title 		<- "Pac-Man Residual of the Mean TPW and Temperature Model\nCondition: Overcast"
        }else{
            x <- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
            y <- log(clear_sky.results$avg, base=exp(1))
            title 		<- "Pac-Man Residual of the Mean TPW and Temperature Model\nCondition: Clear Sky"
        }
        # Finds and removes NaNed values from the dataset
        nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
        x <- x[-(nans)]; y <- y[-(nans)]
        pac.resid(x, y, title, c("Zenith Sky Temperature", "degC"))
        cat(green("[2]"), "Pac-Man Residual Plot\n")
    }

    return(list(pac1(overcast),
                pac2(overcast)))
}

#' @title charts
#' @description Instrument chart
#' @return Instrumentation bar charts
#' @export
charts	<- function(...){
    snsr_sky  <- clear_sky.results$raw_sky
    snsr_skyo <- overcast.results$raw_sky

    par(mar=c(5.1, 5.1, 5.1, 1.3),oma=c(0,0,0,0), xpd=TRUE)
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
			pct 	<- data.frame(B=c(over/length(unlist(snsr_skyo[count]))*100, 
                                      over_na/length(unlist(snsr_skyo[count]))*100, 
                                      over_inf/length(unlist(snsr_skyo[count]))*100),
                                  A=c(norm/length(unlist(snsr_sky[count]))*100, 
                                      norm_na/length(unlist(snsr_sky[count]))*100,
                                      norm_inf/length(unlist(snsr_sky[count]))*100))
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
            slices <- as.matrix(slices)
			axis(side = 2, labels=TRUE, las=1)
			minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
			for (i in 1:3){
                for (j in 1:length(pct)){
                    text(bar[i,j],as.numeric(slices[i,j])+10,
                                labels=sprintf('%s %%', round(as.numeric(pct[i,j]),1)))
                }

			}
	}
}

#' @title poster.plots
#' @description The set of all poster 
#' @return All available poster plots
#' @export
poster.plots <- function(...){
    for (i in 1:length(snsr_name)){
        if(config[[i]]$sensor$poster == FALSE){
            snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
            snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
            snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
            snsr_name[[i]] <- NULL
        }
    }
#' @title poster1
#' @description The time series poster plot 
#' @return A sky temperature time series plot
#' @export
    poster1 <- function(...){
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

        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Sky Temperature",line=0.5)
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

        ticks.at <- seq(as.Date(paste(substr(over_date[[1]], 1, 8),"01",sep="")), over_date[[length(over_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Sky Temperature", line=0.5)
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

        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Ground Temperature", line=0.5)
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

        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Ground Temperature", line=0.5)
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

        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Difference in Temperature", line=0.5)
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

        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Difference in Temperature", line=0.5)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(over_date,range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        mtext("Condition: Overcast", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.76))
        mtext("Condition: Clear Sky", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.26))
        cat(green("[1]"), "Sky-Ground-Delta Temperature Time Series\n")
    }

#' @title poster2
#' @description The analytics poster plot 
#' @return A sky temperature time series plot
#' @export
    poster2 <- function(...){
        ## Layout/Margin Configuration
            par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
            layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
        ## Locational Averagen PW Temperature Correlation
            xmax 	<- max(as.numeric(unlist(clear_sky.results$snsr_sky_calc)), na.rm=TRUE)
            xmin 	<- min(as.numeric(unlist(clear_sky.results$snsr_sky_calc)), na.rm=TRUE)
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))

            range 	<- clear_sky.results$tmp_avg
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

            title("Locational Mean TPW and Temp",line=0.5)

            mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
            if (length(range) >= 2){
                for(j in 2:length(range)){
                    points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
                }
            }
            legend("topleft", legend=unique(pw_time), col=colscheme(range), pch=c(16))

        ## Temporal Average Pw Temperature Correlation
            range 	<- clear_sky.results$loc_avg
            ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE)
            ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)

            plot(x,  t(unlist(range[1])), xlab=NA,
                                          ylab=NA,
                                          xlim=c(xmin, xmax),
                                          ylim=c(ymin, ymax),
                                          main=NA,
                                          pch=16,
                                          col=colscheme(range)[1])

            title("Temporal Mean TPW and Temp",line=0.5)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            if (length(range) >= 2){
                for(j in 2:length(range)){
                    points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
                }
            }
            legend("topleft", legend=unique(pw_place), col=colscheme(range), pch=16)

        ## Total Mean PW Temperature Correlation with exponential regression
            exp_reg <- exp.regression(x, clear_sky.results$avg, clear_sky.results$pw_loc, 1)
        # Non-linear model (exponential)
            plot(exp_reg$x,exp_reg$y, pch=1,
                                      xlim=c(exp_reg$xmin, exp_reg$xmax),
                                      xlab=NA,
                                      ylab=NA,
                                      main=NA)

            title("Mean TPW vs Temp",line=0.5)
            mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
        # Best Fit
        curve(iter.results$A*exp(iter.results$B*x), col="black", add=TRUE)

        # Prediction Interval
            polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]),
                      rev(exp(exp_reg$predint[ ,2]))),
                      col=rgb(0.25, 0.25, 0.25,0.25),
                      border = NA)
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        legend("topleft",col=c("black"), lty=c(1),
        legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f*mm)",
        iter.results$A,iter.results$B,iter.results$S))))
        # Layout configuration for preceding plots
        layout(matrix(c(1), 2, 2, byrow=TRUE))
        cat(green("[2]"), "Analytical Plots\n")
    }

    return(list(poster1(),
                poster2()))
}

#' @title instr
#' @description Plots ground and sky temperature measurements for each individual sensor 
#' @param overcast the condition of the data (clear sky/overcast)
#' @return Instrumentation time series plots
#' @export
instr 	<- function(overcast){
	# X axis limits
	for (count in col_snsr){
		par(mar=c(3,4, 3, 1), oma=c(1,1,0,0), xpd=FALSE)
		layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
		if(overcast){
			date 		<- overcast.results$date
            snsr_sky    <- overcast.results$snsr_sky
            snsr_gro    <- overcast.results$snsr_gro
            sky_title 	<- sprintf("Condition: Overcast \n Sky Temperature Time Series for %s", snsr_tag[count[1]])
			gro_title 	<- sprintf("Ground Temperature Time Series for %s", snsr_tag[count[1]])
		}else{
			date 		<- clear_sky.results$date
            snsr_sky    <- clear_sky.results$snsr_sky
            snsr_gro    <- clear_sky.results$snsr_gro
			sky_title 	<- sprintf("Condition: Clear Sky \n Sky Temperature Time Series for %s", snsr_tag[count[1]])
			gro_title 	<- sprintf("Ground Temperature Time Series for %s", snsr_tag[count[1]])
		}
        sky_range <- snsr_sky[count]
        gro_range <- snsr_gro[count]

		plot(date, t(unlist(sky_range[1])), xlab="Date", ylab="Temperature [C]",
				main=sky_title, pch=16, xaxt='n',
				col=c(snsr_color[count[1]]))
		minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
		mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

		if (length(sky_range) > 1){
			for(j in 2:length(sky_range)){
				points(date,t(unlist(sky_range[j])), pch=16, col=c(snsr_color[count[j]]))
			}
			legend("topleft",col=c(snsr_color[count]), pch=16,
				legend=c(gsub("_", " ",snsr_name[count])))
		}
		plot(date, t(unlist(gro_range[1])), xlab="Date", ylab="Temperature [C]",
				main=gro_title, pch=16, xaxt='n',
				col=c(snsr_color[count[1]]))
		minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
		mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

		if (length(gro_range) > 1){
			for(j in 2:length(gro_range)){
				points(date,t(unlist(gro_range[j])), pch=16, col=c(snsr_color[count[j]]))
			}
		}
	}
}


#' @title heatmp
#' @description A collection of heatmap visualizations
#' @return heatmap visuals
#' @export
heat.maps <- function(date, overcast){
    force(date); force(overcast)

    heat1 <- function(date, overcast){
        # Plotting margins
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		    <- overcast.results$snsr_sky
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Overcast")
        }else{
            range	 		<- clear_sky.results$snsr_sky
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Clear Sky")
        }
        # removes all data that is not defined
        test <- unlist(range[3])
        #test[is.na(unlist(range))] <- 0

        # defines the max and min of the y-axis
        # ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        # plots the first range in the list
        data <- as.numeric(test)
        ones <- as.numeric(as.list(rep(1, length(data))));

        col <-  colorRampPalette(c('red','blue'))
        data.col <- col(length(data))[as.numeric(cut(data,breaks = length(data)))]
        heatplot(data)
        # defines major and minor tick marks for the x-axis and their position
        # ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]] , by = "months")
        # mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        # mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        # axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        # axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        # plots all other ranges in the list if the length of range is greater than 1
        # if (length(range) > 1){
        #     for(i in 2:length(range)){
        #         points(date, t(unlist(range[i])), pch=16, col=snsr_color[i])
        #     }
        # }
        # defines the legend
        # legend("topright", inset=c(-0.21, 0),
        #                    legend=c(gsub("_", " ",snsr_name)),
        #                    col=c(snsr_color),
        #                    pch=c(16,16, 16))
        # print statement when completed
        cat(green("[1]"), "Sky Temperature Heat Map\n")
    }

    heat2 <- function(date, overcast){
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		    <- overcast.results$snsr_sky
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Overcast")
        }else{
            range	 		<- clear_sky.results$snsr_sky
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Clear Sky")
        }
        x <- matrix()
    }


    return(heat1(date, overcast))
}


dev.plots <- function(date, overcast){
    force(date); force(overcast)
#' @title plots5
#' @description Residual Plot
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots5 	<- function(overcast){
        if(overcast){
            title 	<- "Residual of the Mean TPW and Temperature Model \n Condition: Overcast"
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
        }else{
            title 	<- "Residual of the Mean TPW and Temperature Model \n Condition: Clear Sky"
            x 		<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
        }
        # print(resids)
        if (step > 1){
            R		<- Reduce("+", resids)/length(resids)
        } else {
            R <- resids
        }
        print(c(length(x), length(R)))
        plot(x, R, col=c("royalblue"), pch=16,
        ylim=c(min(R), max(R)),
            xlab="Zenith Sky Temperature [C]", ylab=bquote(.("Residual Values [")*sigma*.("]")), main=title)
        abline(h=0, col="gray")
        cat(green("[5]"), "Residual of the Mean PW and Temperature Model\n")
    }
#' @title poster3
#' @description The instrumentation bar charts
#' @return A sky temperature time series plot
#' @export
    poster3 <- function(...){
        out_sky_inf <- inf_counter(bool=TRUE, clear_sky.results$snsr_sky, 'sky')
        out_skyo_inf <- inf_counter(bool=TRUE, overcast.results$snsr_sky, 'skyo')
        snsr_sky_inf <- snsr_skyo_inf <- list()
        for (i in seq(1, length(clear_sky.results$snsr_sky))){
            snsr_sky_inf[[ paste("snsr_sky_inf",i,sep="") ]] <- out_sky_inf[[i]]
            snsr_skyo_inf[[ paste("snsr_skyo_inf",i,sep="") ]] <- out_skyo_inf[[i]]
        }
        title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
        color 	<- c("#FFFFFF", "#000000", "#D6D6D6", "#616161")
        layout(matrix(c(4,1,2,3), 2, 2, byrow=TRUE))
        par(mar=c(0, 2, 4,2), oma=c(2.5,0,0,0.5), xpd=TRUE)
        for(a in 1:length(snsr_name)){
            if ((a/4)%%1 == 0){
                par(oma=c(5, 5, 5, 5), mar=c(5,3,5,5), xpd=NA)
                title("Condition Distribution by Sensor", line=3)
                legend(5, 5,legend = title, fill=color)
                par(mar=c(0, 2, 4,2), oma=c(2.5,0,0,0.5), xpd=TRUE)
            }
            norm	<- length(na.omit(unlist(snsr_sky_inf[a])))
            over	<- length(na.omit(unlist(snsr_skyo_inf[a])))

            norm_na <- length(unlist(snsr_sky_inf[a])) - norm
            over_na <- length(unlist(snsr_skyo_inf[a])) - over

            slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
            pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)

            bar <- barplot(rev(slices), col=rev(color),
            horiz=TRUE, las=1,xlab=NA, axes=FALSE, xlim=c(0, round(max(slices)+50, -2)))
            minor.tick(nx=2, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
            mtext(sprintf("%s", gsub("_", " ",snsr_name[a])), font=2, side=3, line=0)
            mtext("N", side=1, line=1, at=round(max(slices)+50, -2)+35, cex=1)

            axis(side = 1, labels=TRUE, las=1, cex.axis=0.9)
            for (i in 1:length(slices)){
                text(slices[2]*1.5, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
            }
        }
        par(oma=c(5, 5, 5, 5), mar=c(5,3,5,5), xpd=NA)
        title("Condition Distribution by Sensor", line=3)
        legend(5, 5,legend = title, fill=color)
        cat(green("[3]"), "Condiiton Distrbuion by Sensor\n")
    }
    return(plots5(overcast), poster3())
}


data.products <- function(overcast, dir, i){

    data1 <- function(overcast, dir){
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
          cat(green(sprintf("Data sent to data/data_overcast.csv\n")))
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
            cat(green(sprintf("Data sent to %sdata_clear.csv\n", dir)))

        }
}

    data2 <- function(dir){
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
        cat(green(sprintf("Data sent to %sml_data.csv\n", dir)))
        }
    if (i == 'a'){
        return(data1(overcast, dir))
    } else if (i == 'm'){
        return(data2(dir))
    }
}