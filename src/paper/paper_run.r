#' @title Precipitable Water Model Analysis Tool: Offical paper plots
#' @file paper_figures.r
#' @author Spencer Riley
#' @docs https://git.io/fjVHo

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(RColorBrewer); library(plotrix)
library(yaml)
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(pracma))
# suppressPackageStartupMessages(library(gdata))
options(warn=-1)

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Imports data from master_data.csv
fname       <- read.table(file= "../../data/socorro_nm/archive/master_data_paper.csv", sep=",", header=TRUE, strip.white=TRUE)
## Imports sensor information from instruments.txt
config		<- yaml.load_file("../../data/socorro_nm/archive/_pmat.yml")

## Imports data from master_data.csv
fname1       <- read.table(file= "../../data/paper/appendixB.csv", sep=",", header=TRUE, strip.white=TRUE)

daynum <- lapply(fname1[1], as.Date, "%m/%d/%Y")
suomi <- as.numeric(unlist(fname1[2]))
aeronet <- as.numeric(unlist(fname1[3]))
abq_pwv <- as.numeric(unlist(fname1[4]))
epz_pwv <- as.numeric(unlist(fname1[5]))
wt_mean <- as.numeric(unlist(fname1[6]))


# Processing functions
source("../pmat_processing.r")
# Analysis functions
source("../pmat_analysis.r")
overcast.results <- sky.analysis(overcast.filter(col_con, col_date, col_com, pw_name, snsr_name, TRUE))
clear_sky.results <- sky.analysis(overcast.filter(col_con, col_date, col_com, pw_name, snsr_name, FALSE))
iter.results <- iterative.analysis(FALSE, "../../data/socorro_nm/archive/", TRUE)
source("paper_plots.r")

data1 <- function(){
	ml_pw <- list()
	## Average PW
	for(a in 1:length(col_pw)){
		ml_pw[[ paste("ml_pw", a, sep="") ]] <- as.numeric(unlist(fname[col_pw[a]]))
	}
	date 		<- as.Date(fname[ ,col_date], "%m/%d/%Y")

	data 		<- data.frame(list(date=c(date),ml_pw))

	write.fwf(data, file=sprintf("data.csv"), sep="\t", na="NaN", colnames=FALSE)

}

data2 <- function(){
	t <- data.frame(list(date=as.Date(as.numeric(date), origin="1970-01-01"),
					temp=round(as.numeric(temp), 2),
					 model=round(20.202 * exp(0.036 * as.numeric(temp)),2),
					suominet=round(as.numeric(snet)*10, 2),
					aeronet=round(as.numeric(anet)*10, 2)))
	dfm <- within(t, {
		   model <- sprintf("%6.3f",model)
		   temp  <- sprintf("%6.3f",temp)
		   suominet  <-sprintf("%6.3f",suominet)
		   aeronet  <- sprintf("%6.3f",aeronet)
		   })
	write.table(dfm, file="../../data/paper/model.txt", quote=FALSE, row.names=FALSE, sep="\t")
}
temp <- snet <- anet <- date <- list();
for (i in 1:length(unlist(daynum))){
	for (j in 1:length(unlist(clear_sky.results$date))){
		if (daynum$date[i] == clear_sky.results$date[[j]]){
			date <- append(date, as.numeric(clear_sky.results$date[[j]]))
			temp <- append(temp, clear_sky.results$snsr_sky_calc[j])
			snet <- append(snet, suomi[i])
			anet <- append(anet, aeronet[i])
		}
	}
}

pdf("../../figs/paperplots.pdf")
if ("FLIR" %in% snsr_name){
	figure1(clear_sky.results$snsr_sky$snsr_sky2,
			clear_sky.results$snsr_sky$snsr_sky1,
			clear_sky.results$snsr_sky$snsr_sky3,
			clear_sky.results$snsr_gro$snsr_gro2,
			clear_sky.results$snsr_gro$snsr_gro1,
			clear_sky.results$snsr_gro$snsr_gro3)
} else {
	figure2()
	figure3()
	figure6()
	figureA1()
	# figureB1(20.2 * exp(0.036 * as.numeric(temp)), as.numeric(snet) * 10, as.numeric(anet) * 10)
	print(iter.results$M)
	figureB1(iter.results$A * exp(iter.results$B * as.numeric(temp)), as.numeric(snet) * 10, as.numeric(anet) * 10)

	figureB2()
}
