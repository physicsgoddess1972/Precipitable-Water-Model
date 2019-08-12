## Imports data from master_data.csv
fname       <- read.table(file="../data/modtran/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)
rad <- function(data){
    intensity <- data * pi * 10^4
    return(intensity)
}
pdf("~/Downloads/modtran.pdf")
plot(fname[,1], rad(fname[,2]), type='n', xlab=NA, ylab=NA,
main="MODTRAN Plot", xlim=c(5, 30))
lines(fname[,1], rad(fname[,2]), cex=0.6, col="#FF9300")
lines(fname[,1], rad(fname[,3]), cex=0.6, col="#006CFF")
mtext(bquote("Intensity (W/"*m^2*mu*m*")"), side=2, line=2)
mtext(bquote("Wavelength ("*mu*m*")"), side=1, line=3)

# c = (2.998*10^8)*(10^6)
# h = (6.626*10^-34)
# k = (1.38*10^-23)
# T = 280
# print(exp((-h*c)/(5:10 * k * T)))
# curve(exp((-h*c)/(x * k * T)), add=TRUE)

legend("topright", col=c("#FF9300","#006CFF"), pch=c(16,16), legend=c("Water Vapor Scale = 1","Water Vapor Scale = 2"))
graphics.off()