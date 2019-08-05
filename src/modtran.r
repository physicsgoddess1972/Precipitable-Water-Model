## Imports data from master_data.csv
fname       <- read.table(file="../data/modtran.csv", sep=",", header=TRUE, strip.white=TRUE)
plot(fname[,2], fname[,4], pch=16, xlab=sprintf("Wavelength (microns)"), ylab="Radiance (W/CM2-STER-MICRN)",
main="MODTRAN Plot")