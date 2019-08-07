## Imports data from master_data.csv
fname       <- read.table(file="../data/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)
rad <- function(data){
    intensity <- data * pi * 10^4
    return(intensity)
}
pdf("~/Downloads/modtran.pdf")
plot(fname[,1], rad(fname[,2]), pch=16, xlab=sprintf("Wavelength (microns)"), ylab="Intensity (W/M2-MICRN)",
main="MODTRAN Plot", cex=0.6, col="#FF9300", xlim=c(5, 30))
points(fname[,1], rad(fname[,3]), pch=16, cex=0.6, col="#006CFF")
legend("topright", col=c("#FF9300","#006CFF"), pch=c(16,16), legend=c("Water Vapor Scale = 1","Water Vapor Scale = 2"))
graphics.off()