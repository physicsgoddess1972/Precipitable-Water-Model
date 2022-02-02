#' :file: pmat_run.r
#' :module: Precipitable Water Model Analysis Tool
#' :synopsis: Documentation is available docs.pmat.app
#' :author: Spencer Riley

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse)
library(crayon)
library(RColorBrewer)
library(plotrix)
suppressPackageStartupMessages(library(pacviz))
suppressMessages(library(Hmisc))
library(yaml)
options(warn=-1)

## Used for argument parsing run Rscript model.r --help
parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument('--dir', help="Directory path to data folder", default="./data/")
parser$add_argument("-s", "--set", type="character", default=FALSE,
	help="Select plot sets: \\n\  [t]ime series\\n\  [a]nalytics\\n\  [c]harts\\n\  [i]ndividual sensors\\n\  [p]acman\\n\  p[o]ster")
parser$add_argument("-d", "--data", type="character", default=FALSE,
	help="Generates select data sets:\\n\ [m]achine learning\\n\ [a]nalytics")
parser$add_argument("-o", "--overcast", action="store_true", default=FALSE,
	help="Shows plots for days with overcast condition. \\n\ (Used with --set [t/a/i/h/p] and --data [a])")
parser$add_argument("-1st", "--first_time", action="store_true", default=FALSE,
	help="Notes for first time users")
parser$add_argument("-u", action="store_true", default=FALSE,
	help="Determines whether to write a new _output.yml file for analysis")
parser$add_argument("-z", "--dev", action="store_true", default=FALSE)
args <- parser$parse_args()

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Command Prompt "Start of Program" and 1st time user stuff
if(args$first_time){
	message(bold(cloudblue(" \t\t**** Welcome First Time Users ****\t\t\t")))
	message(bold(cloudblue(paste(rep("\t   _  _\t\t\t", 2, collapse="")))))
	message(bold(cloudblue(paste(rep("\t  ( `   )_\t\t", 2, collapse="")))))
	message(bold(cloudblue(paste(rep("\t (     )   `)\t\t",2, collapse="")))))
	message(bold(cloudblue(paste(rep("\t(_   (_ .  _) _)\t", 2, collapse="")))))
	message(bold(cloudblue("Some Notes:")))
	message((green("\t- Arguments: Rscript model.r -h or Rscript model.r --help.")))
	message((yellow("\t- Issues/Bugs?: https://bugs.pmat.app")))
	quit()
}
# Error/Warning definitions
source("./pmat_logging.r")
## Imports sensor information from instruments.txt
if(file.exists(paste(args$dir,"_pmat.yml", sep=""))){
	config		<- yaml.load_file(paste(args$dir,"_pmat.yml", sep=""))
} else {
	error(F02)
}
fig_dir <- "../figs/results/"
source("./pmat_utility.r")
startup()

## Imports data from master_data.csv
if (file.exists(paste(args$dir,"master_data.csv", sep=""))){
	fname       <- read.table(paste(args$dir,"master_data.csv", sep=""), sep=",", header=TRUE, strip.white=TRUE)
} else {
	error(F01)
}
if(file.exists(paste(args$dir,"_output.yml", sep=""))){
	oname <- yaml.load_file(paste(args$dir,"_output.yml", sep=""))
} else {
	warning(f01)
	oname <- file.create(paste(args$dir,"_output.yml", sep=""))
	args$u <- TRUE
}

# Processing functions
source("./pmat_processing.r")
# Analysis functions
source("./pmat_analysis.r")

clear_sky.results <- sky.analysis(overcast.filter(col_con, col_date, col_com,
												  pw_name, snsr_name, FALSE))
overcast.results <- sky.analysis(overcast.filter(col_con, col_date, col_com,
												 pw_name, snsr_name, TRUE))

if(args$set == "a" || args$set == "o"){
	ifelse(args$overcast, 	len <- length(overcast.results$date),
		   					len <- length(clear_sky.results$date))
	if (len > 0){
		iter.results <- iterative.analysis(args$overcast, args$dir, args$u)
	} else {
		error(D01)
	}
}

if (args$set != "c" || args$set != FALSE){
	suppress(message(magenta("Condition: "), ifelse(args$overcast, "Overcast", "Clear Sky")))
}

# Graphical and Data product functions
source("pmat_products.r")

if(length(args$data) > 0){
	data.products(args$overcast, args$dir, args$data);
}
if(args$set == "i"){
	sname_pub <- sprintf("%ssensor%s.pdf", fig_dir, ifelse(args$overcast,"overcast", ""))
	suppress(save(c(instr(overcast=args$overcast)), sname_pub))
}else if(args$set == "t"){
	ifelse(args$overcast, 	date <- overcast.results$date,
		   					date <- clear_sky.results$date)
	ifelse(args$overcast, 	time <- overcast.results$time,
		   					time <- clear_sky.results$time)

	datetime <- as.POSIXct(paste(as.Date(unlist(date), origin="1970-01-01"),
							 	paste(unlist(time),":00", sep="")),
                           format="%Y-%m-%d %H:%M:%S")

	sname_pub <- sprintf("%stime_series%s.pdf", fig_dir, ifelse(args$overcast,"overcast", ""))

	if (length(date) > 0){
		suppress(save(c(time_series.plots(datetime, args$overcast)), sname_pub))
	} else {
		error(D01)
	}
}else if(args$set == "a"){
	sname_pub <- sprintf("%sanalytics%s.pdf", fig_dir, ifelse(args$overcast,"overcast", ""))
	suppress(save(c(analytical.plots(args$overcast)), sname_pub))
}else if(args$set == "c"){
	sname_pub 	<- sprintf("../figs/results/charts.pdf")
	suppress(save(c(charts()), sname_pub))
} else if (args$set == "p") {
	sname_pub <- sprintf("%spacman%s.pdf", fig_dir, ifelse(args$overcast,"overcast", ""))
	suppress(save(c(pac.plots(args$overcast)), sname_pub))
} else if (args$set == "o"){
	sname_pub <- sprintf("%sposter%s.pdf", fig_dir, ifelse(args$overcast,"overcast", ""))
	suppress(save(c(poster.plots(args$overcast)), sname_pub))
}
if (args$dev){
	source("./util/tests/analysis_feat.r")
	source("./util/tests/plots_dev.r")

	save(c(dev.temp(), dev.pw()), sprintf("%sdev.pdf", fig_dir))
}
if (args$set != FALSE){
	suppress(message(green(sprintf("Plot set downloaded to %s\n", sname_pub))))
}
closing()