#' :file: pmat_run.r
#' :module: Precipitable-Water Model Analysis Tool
#' :author: Spencer Riley <sriley@pmat.app>
#' :synopsis: The main file for PMAT. Documentation available at <https://docs.pmat.app>.
#' :author: Spencer Riley

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse)
library(crayon)
library(RColorBrewer)
library(plotrix)
suppressPackageStartupMessages(library(pacviz))
suppressMessages(library(Hmisc))
library(yaml)
library(logging)
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
parser$add_argument("-all", action="store_true", default=FALSE)
args <- parser$parse_args()

# addHandler(writeToFile, logger='', file=paste(args$dir, "testing.log", sep=""))

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

# Error/Warning definitions
source("./pmat_utility.r")

fig_dir <- paste(args$dir, "../figs/results/", sep="")
## Imports sensor information from instruments.txt
if(file.exists(paste(args$dir,"_pmat.yml", sep=""))){
	config		<- yaml.load_file(paste(args$dir,"_pmat.yml", sep=""))
	assign("level", config[[3]]$logging[[1]]$verbose)
} else {
	logg("ERROR", F02); closing()
}

if (args$first){first()}
startup()

## Imports data from master_data.csv
if (file.exists(paste(args$dir,"master_data.csv", sep=""))){
	fname       <- read.table(paste(args$dir,"master_data.csv", sep=""), sep=",", header=TRUE, strip.white=TRUE)
} else {
	logg("ERROR", F01); closing()
}
## Tries to read _output.yml
if(file.exists(paste(args$dir,"_output.yml", sep=""))){
	oname <- yaml.load_file(paste(args$dir,"_output.yml", sep=""))
} else {
	logg("WARN", f01)
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

# Graphical and Data product functions
source("pmat_products.r")

if(args$set != FALSE){
	visual.products(args$set)
}


if(length(args$data) > 0){
	data.products(args$overcast, args$dir, args$data);
}


if (args$all){
	opt <- c('t', 'a', 'i', 'c', 'o', 'p')
	logg("INFO", paste("Condition:", ifelse(args$overcast, "Overcast", "Clear Sky"), sep=" "))
	for (i in opt){
		visual.products(i, args$overcast)
	}

}

if (args$dev){
	source("./util/tests/analysis_feat.r")
	source("./util/tests/plots_dev.r")

	save(c(dev.temp(), dev.pw()), sprintf("%sdev.pdf", fig_dir))
}

closing()