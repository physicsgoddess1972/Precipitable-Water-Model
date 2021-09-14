####
## Title: 	Precipitable Water Model
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [Rscript model.r --help]
####

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(RColorBrewer); library(plotrix)
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(pracma))

## Imports data from master_data.csv
fname       <- read.table(file= "appendixB.csv", sep=",", header=TRUE, strip.white=TRUE)

daynum <- as.numeric(unlist(fname[1]))
suomi <- as.numeric(unlist(fname[2]))
aeronet <- as.numeric(unlist(fname[3]))
abq_pwv <- as.numeric(unlist(fname[4]))
epz_pwv <- as.numeric(unlist(fname[5]))
wt_mean <- as.numeric(unlist(fname[6]))

pdf("compare_r_a_s.pdf")
plot(daynum, suomi, pch=16, col="red", ylim=c(0, 4), xlim=c(0, 365),xaxt='n', yaxt='n',yaxs="i",xaxs="i", xlab="Day Number (2020)", ylab="PWV (cm)", main="PWV Comparison 2020")
points(daynum, aeronet, pch=16, col="orange")
points(daynum, abq_pwv, pch=16, col="green4")
points(daynum, epz_pwv, pch=16, col="steelblue3")
lines(daynum, wt_mean, lty="solid", col="black")
minor.tick(nx=5, ny=10, tick.ratio=0.5, x.args = list(), y.args = list())
x_ticks.at <- seq(0, 365, by = 50)
x_mj_ticks <- x_ticks.at[seq(1, length(x_ticks.at), length.out=8)]
axis(1, at=x_mj_ticks, labels=x_mj_ticks, tck=-0.02)

y_ticks.at <- seq(0.0, 4.0, by = 0.5)
y_mj_ticks <- y_ticks.at[seq(1, length(y_ticks.at), length.out=9)]
axis(2, at=y_mj_ticks, labels=sprintf("%3.1f", y_mj_ticks), tck=-0.02)


legend("topleft", col=c("red", "orange", "green4", "steelblue3", "black"), lty=c(0,0,0,0,1), pch=c(16,16,16,16,NA), legend=c("SuomiNet", "AERONET", "ABQ Sonde", "EPZ Sonde", "Sonde Mean"))