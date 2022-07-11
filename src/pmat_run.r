
#' :file: pmat_run.r
#' :module: Precipitable-Water Model Analysis Tool
#' :author: Spencer Riley <sriley@pmat.app>
#' :synopsis: The main file for PMAT. Documentation available at <https://docs.pmat.app>.

# Necessary Libraries for the script to run
# Use the DESCRIPTION file for installation
library(argparse)
library(crayon)
library(RColorBrewer)
library(plotrix)
suppressPackageStartupMessages(library(pacviz))
suppressMessages(library(Hmisc))
suppressPackageStartupMessages(library(e1071))
library(yaml)
options(warn=-1)

# Used for argument parsing run Rscript model.r --help
parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument('--dir', help="Directory path to data folder", default="./data/")
parser$add_argument('--out', help="Directory path to out folder")
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

# Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

# Defines short-hand for directory names
src.dir <- 	file.path(args$dir)
out.dir <- 	file.path(args$out)
if (grepl("//", out.dir, fixed=TRUE)){
	out.dir <- gsub("//", "/", out.dir)
}
fig.dir <- 	file.path(out.dir, "figs/")
dat.dir <-	file.path(out.dir, "data/")
dir.create(out.dir)
dir.create(fig.dir)
dir.create(dat.dir)
# Error/Warning definitions
err_war = yaml.load_file(file.path("./src/pmat_codes.yml"))
err = err_war$error
warn = err_war$warn

# Imports utility functions
source("./src/pmat_utility.r")

# Checks to see if _pmat.yml is present
# Throws error if the file is not in the expected directory
if(file.exists(file.path(src.dir, "_pmat.yml"))){
	config	<- yaml.load_file(file.path(src.dir, "_pmat.yml"))
	level 	<- config[[3]]$logging[[1]]$verbose
} else {
	logg("ERROR", err$F[[1]]$code, lev = "DEBUG")
    logg("ERROR", err$F[[1]]$fix, lev = "DEBUG")
	aloha.closing()
}

# Checks if -1st argument is present
if (args$first){aloha.first()}

# Startup CLI messages
aloha.startup()

# Checks to see if cool_data.csv is present
# Throws error if the file is not in the expected directory
if (!file.exists(file.path(src.dir, "cool_data.csv"))) {
    logg("ERROR", err$F[[2]]$code, lev = "DEBUG")
    logg("ERROR", err$F[[2]]$fix, lev = "DEBUG")
	aloha.closing()
}

# Imports data from master_data.csv
if (!file.exists(file.path(src.dir, "master_data.csv"))){
	logg("WARN",warn$f[[1]]$code, lev = level)
	logg("WARN",warn$f[[1]]$fix, lev = level)
	fname <- file.create(file.path(src.dir, "master_data.csv"))
}
# Tries to read _output.yml
if(!file.exists(file.path(dat.dir, sprintf("_output%s.yml", ifelse(args$overcast,"_overcast", ""))))){
	logg("WARN",warn$f[[1]]$code, lev = level)
	logg("WARN",warn$f[[1]]$fix, lev = level)
	out.name <- file.create(file.path(dat.dir, sprintf("_output%s.yml", ifelse(args$overcast,"_overcast", ""))))
	args$u <- TRUE
}

# Processing functions
source("./src/pmat_processing.r")

# Pulls only data labeled "clear sky"
filter.overcast.clear <- overcast.filter(col_con,
								col_date,
								col_com,
							  	pw_name,
								snsr_name,
								FALSE)
# Pulls only data labeled "overcast"
filter.overcast.over <- overcast.filter(col_con,
								col_date,
								col_com,
							  	pw_name,
								snsr_name,
								TRUE)
# Results of preprocessing for clear sky data
clear_sky.data 	<- sky.processing(dna.filter(filter.overcast.clear))
# Results of preprocessing for overcast data
overcast.data 	<- sky.processing(dna.filter(filter.overcast.over))

# Easy solution to only get either overcast/clear sky data based on user configuration
if (args$overcast){
	res <- overcast.data
} else {
	res <- clear_sky.data
}
# Converts datetime into a POSIX object
datetime <- as.POSIXct(paste(as.Date(unlist(res$date), origin="1970-01-01"), paste(unlist(res$time),":00", sep="")), 
						format="%Y-%m-%d %H:%M:%S", 
						tz = "MST")
# Pulls values labeled NaN out
nan.out <- nan.filter(list(x=res$snsr_sky_calc,
						   y=res$wt_avg,
						   y1=res$avg,
						   z=res$pw_loc))

# Applies the standard-deviation filter
filter.mean	<- mean.filter(nan.out, rel_diff)

# Analysis functions
source("./src/pmat_analysis.r")
# Visual and Data products functions
source("./src/pmat_products.r")

# Checks to see if there is any data available
if (length(res$date) > 0){
    # Runs the Iterative Analysis and generates the step-based results data file
	iter.results <- iterative.analysis(out.bool=args$u,
									   mean.out = filter.mean)
    # Generates a data file based on the averages of the steps
	data.final(out.dir,
				list(length(clear_sky.data$date),
				     length(overcast.data$date),
				     iter.results$train.len,
				     length(iter.results$nan.out)),
                length(iter.results$filter.mean[[1]])/(length(clear_sky.data$date) - length(iter.results$nan.out)),
				list(A=iter.results$A, B=iter.results$B),
			   	iter.results$S,
				list(M=iter.results$M, K=iter.results$K, R=iter.results$R))
} else {
    # Kills the program if there is not sufficient data
	logg("ERROR", err$D[[1]]$code, lev = "DEBUG")
    logg("ERROR", err$D[[1]]$fix, lev = "DEBUG")
	aloha.closing()
}

# Checks to see if --set parameters are present
if(args$set != FALSE){
	visual.products(args$set, filter.mean, datetime)
	logg("PASS", sprintf("Plot set downloaded to %s", fig.dir), lev = level)
}

# Checks to see if --data parameters are available
if(length(args$data) > 0){
	if (args$data == 'a'){
	    # Generates a data file with weighted mean PWV and mean temperature for
	    # either clear sky or overcast based on user configuration
        data.gen(args$overcast, dat.dir)
    } else if (args$data == 'm'){
        # Generates a data file for machine learning algorithms
       data.ml(fig.dir)
    }
}

# Checks to see if -all argument is present
if (args$all){
	opt <- c('t', 'a', 'i', 'c', 'o', 'p')
	logg("INFO", paste("Condition:", ifelse(args$overcast, "Overcast", "Clear Sky"), sep=" "))
	for (i in opt){
		visual.products(i, filter.mean, datetime, args$overcast)
		logg("PASS", sprintf("Plot set downloaded to %s", fig.dir), lev = level)
	}
	data.gen(args$overcast, dat.dir)
	data.ml(dat.dir)
}

# Checks to see if -z (development) argument is present
# **This may not work on you machine if the following files are not present:**
# - analysis_feat.r
# - plots_dev.r
if (args$dev){
	source("./util/dev/analysis_feat.r")
	source("./util/dev/plots_dev.r")
	pdf(sprintf("%sdev.pdf", fig.dir))
	dev.plots()
}

# Ends the program
aloha.closing()
