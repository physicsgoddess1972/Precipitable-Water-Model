## Imports data from master_data.csv
fname       <- read.table(file="../data/modtran/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)

modtran_plot <- function(xran){
    rad <- function(data){
        intensity <- data * pi * 10^4
        return(intensity)
    }
    color = c("#00BCD7", "#FF9A00","#D90000", "#8F00C9")
    plot(fname[,1], rad(fname[,2]), type='n', xlab=NA, ylab=NA,
    main="MODTRAN Plot", xlim=xran, ylim=c(0, 30))
    lines(fname[,1], rad(fname[,2]), cex=0.6, col=color[1])
    lines(fname[,1], rad(fname[,3]), cex=0.6, col=color[2])
    mtext(bquote("Intensity (W/"*m^2*mu*m*")"), side=2, line=2)
    mtext(bquote("Wavelength ("*mu*m*")"), side=1, line=3)

    planck_curve <- function(x, T){
        c1  = 1.191042*10^8
        c2  = 1.4387752*10^4
        B   = c1/(x^5*(exp(c2/(x*T)) - 1)) * pi
        return(B)
    }
    curve(planck_curve(x, 240), col=color[3], add=TRUE)
    curve(planck_curve(x, 280),col=color[4], add=TRUE)

    avg_1 <- avg_2 <- avg_0 <- list()
    for(i in 1:length(fname[,1])){
        if(fname[i,1] >= xran[1]){
            if(fname[i,1] <= xran[2]){
                avg_0 <- append(avg_0, fname[i,1])
                avg_1 <- append(avg_1, rad(fname[i,2]))
                avg_2 <- append(avg_2, rad(fname[i,3]))
            }
        }
    }
    avg0 <- Reduce("+", avg_0)/length(avg_0)
    avg1 <- Reduce("+", avg_1)/length(avg_1)
    avg2 <- Reduce("+", avg_2)/length(avg_2)

    points(avg0, avg1, pch=16, col=color[1])
    points(avg0, avg2, pch=16, col=color[2])
    points(avg0, Reduce("+", planck_curve(xran, 240))/length(planck_curve(xran, 240)), pch=16, col=color[3])
    points(avg0, Reduce("+", planck_curve(xran, 280))/length(planck_curve(xran, 280)), pch=16, col=color[4])

    legend("topright", col=color, pch=c(16,16),
        legend=c("Water Vapor Scale = 1","Water Vapor Scale = 2", "T = 240 K", "T = 280 K"))
}
pdf("~/Downloads/modtran.pdf")
#modtran_plot(c(5,25))

modtran_plot(c(7, 10))
graphics.off()
