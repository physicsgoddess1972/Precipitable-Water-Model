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
suppressPackageStartupMessages(library(e1071))
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
parser$add_argument("-all", action="store_true", default=FALSE)
args <- parser$parse_args()

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

out.dir <- paste(args$dir, "../out/", sep="")
fig.dir <- paste(out.dir, "figs/", sep="")
dat.dir <- paste(out.dir, "data/", sep="")
# Error/Warning definitions
source("./pmat_utility.r")

## Imports sensor information from instruments.txt
if(file.exists(paste(args$dir,"_pmat.yml", sep=""))){
	config	<- yaml.load_file(paste(args$dir,"_pmat.yml", sep=""))
	level 	<- config[[3]]$logging[[1]]$verbose
} else {
	logg("ERROR", F02, lev = "DEBUG"); closing()
}
if (args$first){first()}
startup()

## Imports data from master_data.csv
if (file.exists(paste(args$dir,"master_data.csv", sep=""))){
	fname       <- read.table(paste(args$dir,"master_data.csv", sep=""), sep=",", header=TRUE, strip.white=TRUE)
} else {
	logg("ERROR", F01, lev = level); closing()
}
## Tries to read _output.yml
if(file.exists(paste(out.dir,"data/_output.yml", sep=""))){
	oname <- yaml.load_file(paste(out.dir,"data/_output.yml", sep=""))
} else {
	logg("WARN", f01, lev = level)
	oname <- file.create(paste(out.dir,"data/_output.yml", sep=""))
	args$u <- TRUE
}

# Processing functions
source("./pmat_processing.r")
filter.overcast.clear <- overcast.filter(col_con,
								col_date,
								col_com,
							  	pw_name,
								snsr_name,
								FALSE)

filter.overcast.over <- overcast.filter(col_con,
								col_date,
								col_com,
							  	pw_name,
								snsr_name,
								TRUE)

clear_sky.data 	<- sky.processing(dna.filter(filter.overcast.clear))
overcast.data 	<- sky.processing(dna.filter(filter.overcast.over))

if (args$overcast){
	res <- overcast.data
} else {
	res <- clear_sky.data
}
nan.out <- nan.filter(list(x=res$snsr_sky_calc,
						   y=res$wt_avg,
						   y1=res$avg,
						   z=res$pw_loc))
filter.mean	<- mean.filter(nan.out[[1]]$z, rel_diff)

datetime <- as.POSIXct(paste(as.Date(unlist(res$date), origin="1970-01-01"), paste(unlist(res$time),":00", sep="")), format="%Y-%m-%d %H:%M:%S")

# Analysis functions
source("pmat_analysis.r")
source("pmat_products.r")

if (length(res$date) > 0){
	iter.results <- iterative.analysis(results=res,
									   dir=out.dir,
									   obool=args$u,
									   nan.out = nan.out,
									   mean.out = filter.mean)
	data.final(out.dir,
				length(clear_sky.data$date),
				length(overcast.data$date),
				iter.results$train.len,
				length(nan.out[[2]]),
				length(filter.mean)/(length(clear_sky.data$date) - length(nan.out[[2]])),
				list(A=iter.results$A, B=iter.results$B),
			   	iter.results$S,
				list(M=iter.results$M, K=iter.results$K, R=iter.results$R))
} else {
	logg("ERROR", D01, lev = level); closing()
}


if(args$set != FALSE){
	visual.products(args$set, nan.out, filter.mean, datetime)
	logg("PASS", sprintf("Plot set downloaded to %s", fig.dir), lev = level)
}

if(length(args$data) > 0){
	if (args$data == 'a'){
        data.gen(args$overcast, dat.dir)
    } else if (args$data == 'm'){
       data.ml(fig.dir)
    }
}
if (args$all){
	opt <- c('t', 'a', 'i', 'c', 'o', 'p')
	logg("INFO", paste("Condition:", ifelse(args$overcast, "Overcast", "Clear Sky"), sep=" "))
	for (i in opt){
		visual.products(i, nan.out, filter.mean, datetime, args$overcast)
		logg("PASS", sprintf("Plot set downloaded to %s", fig.dir), lev = level)
	}
	data.gen(args$overcast, dat.dir)
	data.ml(dat.dir)
}
if (args$dev){
	source("./util/dev/analysis_feat.r")
	source("./util/dev/plots_dev.r")
	pdf(sprintf("%sdev.pdf", fig.dir))
	dev.plots()
}
closing()