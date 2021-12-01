#' @title Precipitable Water Model Analysis Tool: Offical paper plots
#' @file paper_figures.r
#' @author Spencer Riley
#' @docs https://git.io/fjVHo

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse)
library(crayon)
library(RColorBrewer)
library(plotrix)
suppressPackageStartupMessages(library(pacviz))
suppressMessages(library(Hmisc))
library(yaml)
options(warn=-1)
## Imports data from master_data.csv
fname       <- read.table(file= "../../data/socorro_nm/archive/master_data_archive.csv", sep=",", header=TRUE, strip.white=TRUE)
## Imports sensor information from instruments.txt
# sensor 		<- suppressWarnings(read.csv(file="./instruments.conf", sep=","))
config		<- yaml.load_file("../../data/socorro_nm/archive/_pmat.yml")
source("../pmat_preprocess.r")
overcast <- overcast.filter(col_con,
							col_date,
							col_com,
							pw_name,
							snsr_name)
source("../pmat_analysis.r")
clear_sky.results <- clear_sky.analysis(overcast)
snsr_sky_calc <- list()
snsr_sky <- list(clear_sky.results$snsr_sky$snsr_sky2,
				 clear_sky.results$snsr_sky$snsr_sky3)
for (i in snsr_sky){
	for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
			append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]],
				   values=na.omit(c(i[j])))
	}
}
for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
	snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
}
for (i in snsr_sky){
	for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
			append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
	}
}
for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
	snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
}
data_indx <- mean.filter(clear_sky.results$pw_loc,
						 clear_sky.results$avg,
						 rel_diff)
data_split <- data.partition(as.numeric(unlist(snsr_sky_calc))[data_indx],
							 clear_sky.results$avg[data_indx],
							 train_size=train_frac*0.01)

train <- data_split$train
exp_reg <- exp.regression(train$x, train$y)
# Non-linear model (exponential)
# Best Fit
# curve(30.55 * exp(x/28.725) - 2.63, col="red", add=TRUE)
test <- data_split$test
est     <- exp(coef(exp_reg$model)[1]+coef(exp_reg$model)[2]*test$x)
# accuracy of model
acc     <- sqrt((1/length(test$x))*(sum((est-test$y)^2)/length(test$x)))
# Residual Standard Deiviation
S       <- sqrt(sum((est-test$y)^2)/(length(test$x) - 2))
# Root Square Mean Error
rsme    <- sqrt(sum((est-test$y)^2)/length(test$x))

mims_y  <- (30.55 * exp(exp_reg$x/28.725) - 2.63)
mims_rsme    <- sqrt(sum((mims_y - exp_reg$y)^2)/length(exp_reg$y))


cat(unlist(list(round(exp_reg$rsme,3),
				round(mims_rsme,3),
				round(as.numeric(coef(exp_reg$model)[1]),3),
				round(as.numeric(coef(exp_reg$model)[2]),3),
				round(as.numeric(exp_reg$S),3),
				round(as.numeric(S), 3),
				round(as.numeric(rsme), 3),
				round(as.numeric(acc), 3),
				'\n')))
