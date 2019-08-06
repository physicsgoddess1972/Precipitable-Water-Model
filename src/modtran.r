## Imports data from master_data.csv
fname       <- read.table(file="../data/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)
plot(fname[,2], fname[,4]*pi*10000, pch=16, xlab=sprintf("Wavelength (microns)"), ylab="Radiance (W/M2-MICRN)",
main="MODTRAN Plot", cex=0.6)