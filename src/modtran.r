## Imports data from master_data.csv
fname1       <- read.table(file="../data/modtran/radiance/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)
fname2       <- read.table(file="../data/modtran/temp_offset_wvs1/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)
fname3       <- read.table(file="../data/modtran/modtran_wps1.csv", sep=",", header=TRUE, strip.white=TRUE)
fname4       <- read.table(file="../data/modtran/temp_offset_wsv_half/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)

modtran_plot <- function(xran,fname,offset, leg, xran1, fname1, offset1, leg1) {
    planck_curve <- function(x, T){
        c1  = 1.191042*10^8
        c2  = 1.4387752*10^4
        B   = c1/(x^5*(exp(c2/(x*T)) - 1)) * pi
        return(B)
    }
    temp_planck_curve <- function(x, B){
        c1  = 1.191042*10^8
        c2  = 1.4387752*10^4
        t   = (x/c2) * log(((c1*pi)/(B*x^5)) + 1)
        T   = 1/t
        return(T)
    }
    rad <- function(data){
        intensity <- data * pi * 10^4
        return(intensity)
    }

    color = c("#0055D788", "#FF880088", "#AB2E8788", "#D90000", "#8F00C9", "#FF3383")

    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
    layout(matrix(c(1,2,1,2), 2, 2, byrow=TRUE))

    plot(fname[,1], rad(fname[,2]), type='n', xlab=NA, ylab=NA,
    main=NA, xlim=xran, ylim=c(0, 30))

    lines(fname[,1], rad(fname[,2]), col=color[1], cex=0.6, lty=1)
    lines(fname[,1], rad(fname[,3]), col=color[2], cex=0.6, lty=1)
    lines(fname[,1], rad(fname[,4]), col=color[3], cex=0.6, lty=1)

    mtext(bquote("Intensity (W/"*m^2*mu*m*")"), side=2, line=2)
    mtext(bquote("Wavelength ("*mu*m*")"), side=1, line=3)

    avg_4 <- avg_3 <- avg_1 <- avg_2 <- avg_0 <- list()
    for(i in 1:length(fname[,1])){
        if(fname[i,1] >= xran[1]){
            if(fname[i,1] <= xran[2]){
                avg_0 <- append(avg_0, fname[i,1])
                avg_1 <- append(avg_1, rad(fname[i,2]))
                avg_2 <- append(avg_2, rad(fname[i,3]))
                avg_3 <- append(avg_3, rad(fname[i,4]))
            }
        }
    }
    avg0 <- Reduce("+", avg_0)/length(avg_0)
    avg1 <- Reduce("+", avg_1)/length(avg_1)
    avg2 <- Reduce("+", avg_2)/length(avg_2)
    avg3 <- Reduce("+", avg_3)/length(avg_3)

    curve(planck_curve(x, temp_planck_curve(avg0, avg1)), add=TRUE)
    curve(planck_curve(x, temp_planck_curve(avg0, avg2)), add=TRUE)
    curve(planck_curve(x, temp_planck_curve(avg0, avg3)), add=TRUE)

    print(list(avg0, avg1))
    print(list(avg0, avg2))
    print(list(avg0, avg3))
    points(avg0, avg1, pch=16, col=substr(color[1], start=1, stop=7))
    points(avg0, avg2, pch=16, col=substr(color[2], start=1, stop=7))
    points(avg0, avg3, pch=16, col=substr(color[3], start=1, stop=7))

    text(9.5,offset[1], label=paste("T = ", round(temp_planck_curve(avg0, avg1), 2), "K"), srt=15, cex=0.75)
    text(9.5,offset[2], label=paste("T = ", round(temp_planck_curve(avg0, avg2), 2), "K"), srt=15, cex=0.75)
    text(9.5,offset[3], label=paste("T = ", round(temp_planck_curve(avg0, avg3), 2), "K"), srt=15, cex=0.75)

    legend("topright", col=c(substr(color[1], start=1, stop=7),
                             substr(color[2], start=1, stop=7),
                             substr(color[3], start=1, stop=7)), lty=c(0,0,0), pch=c(16,16,16), legend=c("                             ","                     ","                     "), bty="n")
    legend("topright", col=c(color[1], color[2], color[3]), lty=c(1,1,1), pch=c("","",""), legend=leg)

    plot(fname1[,1], rad(fname1[,2]), type='n', xlab=NA, ylab=NA,
    main=NA, xlim=xran, ylim=c(0, 30))

    lines(fname1[,1], rad(fname1[,2]), cex=0.6, col=color[1])
    lines(fname1[,1], rad(fname1[,3]), cex=0.6, col=color[2])
    lines(fname1[,1], rad(fname1[,4]), cex=0.6, col=color[3])

    mtext(bquote("Intensity (W/"*m^2*mu*m*")"), side=2, line=2)
    mtext(bquote("Wavelength ("*mu*m*")"), side=1, line=3)

    avg_4 <- avg_3 <- avg_1 <- avg_2 <- avg_0 <- list()
    for(i in 1:length(fname1[,1])){
        if(fname1[i,1] >= xran[1]){
            if(fname1[i,1] <= xran[2]){
                avg_0 <- append(avg_0, fname1[i,1])
                avg_1 <- append(avg_1, rad(fname1[i,2]))
                avg_2 <- append(avg_2, rad(fname1[i,3]))
                avg_3 <- append(avg_3, rad(fname1[i,4]))
            }
        }
    }
    avg0 <- Reduce("+", avg_0)/length(avg_0)
    avg1 <- Reduce("+", avg_1)/length(avg_1)
    avg2 <- Reduce("+", avg_2)/length(avg_2)
    avg3 <- Reduce("+", avg_3)/length(avg_3)

    curve(planck_curve(x, temp_planck_curve(avg0, avg1)), add=TRUE)
    curve(planck_curve(x, temp_planck_curve(avg0, avg2)), add=TRUE)
    curve(planck_curve(x, temp_planck_curve(avg0, avg3)), add=TRUE)

    points(avg0, avg1, pch=16, col=substr(color[1], start=1, stop=7))
    points(avg0, avg2, pch=16, col=substr(color[2], start=1, stop=7))
    points(avg0, avg3, pch=16, col=substr(color[3], start=1, stop=7))

    text(9.5,offset1[1], label=paste("T = ", round(temp_planck_curve(avg0, avg1), 2), "K"), srt=18, cex=0.75)
    text(9.5,offset1[2], label=paste("T = ", round(temp_planck_curve(avg0, avg2), 2), "K"), srt=18, cex=0.75)
    text(9.5,offset1[3], label=paste("T = ", round(temp_planck_curve(avg0, avg3), 2), "K"), srt=18, cex=0.75)

    legend("topright", col=c(substr(color[1], start=1, stop=7),
                             substr(color[2], start=1, stop=7),
                             substr(color[3], start=1, stop=7)), lty=c(0,0,0), pch=c(16,16,16), legend=c("                     ","                     ","                     "), bty="n")
    legend("topright", col=c(color[1], color[2], color[3]), lty=c(1,1,1), pch=c("","",""), legend=leg1)
}

modtran_plot1 <- function(fname){
    plot(fname[,5], fname[,2], xlab="Water Pressure (mbar)", ylab="Altitude (km)",
    main="Altitude and Water Pressure Correlation")
}
pdf("~/Downloads/modtran.pdf")
modtran_plot(c(7,10), fname1, c(10,12.74,15),
  c("TPW = 5.7 mm","TPW = 11.4 mm", "TPW = 22.7 mm"),c(7,10), fname2, c(11.5, 12.75, 14), c("-5 K Offset", "0 K Offset", "+5 K Offset"))
# modtran_plot(c(7,10), fname4, c(10, 11.25, 12.5), c("-5 K Offset", "0 K Offset", "+5 K Offset"))

graphics.off()
