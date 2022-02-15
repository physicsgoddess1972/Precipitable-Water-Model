#' :file: pmat_processing.r
#' :module: Precipitable Water Model Analysis Tool: Pre-processing
#' :synopsis: functions for preprocessing
#' :author: Spencer Riley

system(sprintf("python3 pmat_import.py %s", args$dir))

## Pulls most recent data stamp for the purpose of adding date stamps to file names when plots are saved
recent 		<- t(fname[1])[length(t(fname[1]))]

## Pulls the column number of the first Sky Temperature measurement
col_sky 	<- grep("Sky", colnames(fname))
## Pulls the column number of the first Ground Temperature measurement
col_gro		<- grep("Ground", colnames(fname))
## Pulls the column number of the first PW measurement
col_pw 		<- grep("PW", colnames(fname))
## Pulls the column number of the date
col_date 	<- grep("Date", colnames(fname))
## Pulls the column number of measurement time
col_time 	<- grep("Time", colnames(fname))
## Pulls the column number of the Relative Humidity
col_rh 		<- grep("RH", colnames(fname))
## Pulls the column number of the Relative Humidity
col_dew 	<- grep("Dewpoint", colnames(fname))
## Pulls the column number of the non-measurement temperature
col_temp 	<- grep("Temp", colnames(fname))
## Pulls the column number of the Condition
col_con 	<- grep("Condition", colnames(fname))
## Pulls the column number for the comments
col_com 	<- grep("comments", colnames(fname))
## The value for the training fraction
train_frac 	<- config[[2]]$analysis[[1]]$train_fraction
## The value for the threshold of the mean.filter
rel_diff 	<-  config[[2]]$analysis[[2]]$rel_difference

step 		<-  config[[2]]$analysis[[3]]$iteration$step

import_src 	<- list()
for (i in 1:length(config[[4]]$import)){
	import_src <- append(import_src, names(config[[4]]$import[[i]]))
}

weights 	<- c()
if ("wyoming" %in% import_src){
	wy_idx <- which(import_src == "wyoming")
	for (i in 1:length(config[[4]]$import[[wy_idx]]$wyoming)){
		weights <- append(weights, as.numeric(unlist(config[[4]]$import[[wy_idx]]$wyoming[[i]]$weight)))
	}
}

if ("external" %in% import_src){
	ex_idx <- which(import_src == "external")
	for (i in 1:length(config[[4]]$import[[ex_idx]]$external)){
		weights <- append(weights, as.numeric(unlist(config[[4]]$import[[ex_idx]]$external[[i]]$weight)))
	}
}

## Pulls sensor labels and colors from instruments.txt
snsr_name 	<- list(); snsr_color <- snsr_sky_indx <- snsr_gro_indx  	<- unlist(list())
for(i in 1:length(config[[1]]$instruments)){
	if (!(length(config[[1]]$instruments[[i]]$sensor$active) == 0)){
		if (config[[1]]$instruments[[i]]$sensor$active == TRUE){
				var 				<- assign(paste("Thermo", i, sep=""),
											 config[[1]]$instruments[[i]]$sensor$name)
				snsr_name 			<- append(snsr_name, substr(toString(var), 1, 7))
				snsr_color 			<- append(snsr_color, paste("#", toString(config[[1]]$instruments[[i]]$sensor$color), sep=""))
				snsr_sky_indx 		<- append(snsr_sky_indx, col_sky[i])
				snsr_gro_indx 		<- append(snsr_gro_indx, col_gro[i])
		}
	}
}
temp_name <- list()
temp_gro_indx <- temp_sky_indx <- unlist(list())
for (i in col_temp){
		name 			<- gsub("Temp", "", colnames(fname)[i])
		name 			<- trimws(gsub("[[:punct:]]", " ", name), which="l")
		temp_name <- append(temp_name, name)

		if (grepl("Ground", name)){temp_gro_indx <- append(temp_gro_indx, i)}
		if (grepl("Sky", name)){temp_sky_indx <- append(temp_sky_indx, i)}
}
temp_place <- gsub("_.*$", "", gsub(" ", "_", temp_name))
## Pulls individual PW measurement labels
pw_name 	<- col_pwpl  <-	col_pwtm <- list()
for (j in col_pw){
	name 	<- gsub("PW", "", colnames(fname)[j])
	name 	<- trimws(gsub("[[:punct:]]", " ", name), which="l")
	pw_name <- append(pw_name, substr(name, 1, 6))
}
# Pull general location tag from the label
pw_place 	<- gsub("_.*$", "", gsub(" ", "_", pw_name))
# Pulls general time tag from label
pw_time 	<- gsub("*..._", "", gsub(" ", "_", pw_name))
# Pulls the column numbers that have the general location tag
for (j in pw_place){
	col_pwpl <- append(col_pwpl, list(grep(j, pw_place)))
}
col_pwpl <- unique(col_pwpl)
# Pulls the column numbers that have the general time tag
for (j in unique(pw_time)){
	col_pwtm <- append(col_pwtm, list(grep(j, pw_time)))
}
col_pwtm <- unique(col_pwtm)
# Assigns a color for each label
#pw_color <- distinctColorPalette(length(pw_name), runTsne=TRUE, altCol=TRUE)
pw_color <- brewer.pal(length(pw_name),"Set1")
temp_col <- brewer.pal(length(temp_name), "Spectral")

colscheme <- function(range){
	#' :detail: a function that generates an array of colors based on the number of elements
	#' :param list range: a list of data series
	#' :return: a list of colors
	#' :rtype: list
	col <- brewer.pal(length(range)+1, "Set1")
	return(col)
}
## Pull general location tag from the label
snsr_tag 	<- gsub("*_.", "", snsr_name)
## Pulls the column numbers that have the general location tag
col_snsr <- list()
for (j in unique(snsr_tag)){
	col_snsr <- append(col_snsr, list(grep(j, snsr_tag)))
}

mean.filter <- function(pw, n){
	#' :detail: filters the data based on the comparison of the daily std and the average std of the dataset
	#' :param pw: precipitable water data
	#' :param n: threshold
	#' :return: an array of indicies for PWV values to be analyzed
	#' :rtype: list
	bad <- good <- list()
	out <- std.i <- list()
	for (i in 1:length(pw)){
		for (j in 1:length(pw[[i]])){
			out[[ paste("out",j,sep="") ]] <- append(out[[ paste("out",j,sep="") ]],
													 pw[[ paste("pw_loc", i, sep="" )]][j])
		}
	}
	for (i in 1:length(pw[[1]])){
		std.i <- append(std.i, sd(unlist(out[[ paste("out",i,sep="") ]])))
	}
	std.i <- as.numeric(std.i)
	std.avg <- Reduce("+", na.omit(std.i))/length(na.omit(std.i))
	for (i in 1:length(std.i)){
		if(is.na(std.i[i])){
			next
		} else if (std.i[[i]] - (n * std.avg) < 0){
			good <- append(good, i)
		} else {
			bad <- append(bad, i)
		}
	}
	return(unlist(good))
}

data.partition <- function(x,y, train_size=0.7){
	#' :detail: splits the data into a training/testing set
	#' :param list x: domain of the data
	#' :param list y: range of the data
	#' :param double train_size: fraction of the data in the testing set
	#' :return: a list containing the training and testing sets
	#' :rtype: list
  train_idx <- sample(1:length(x), trunc(length(x)*train_size), replace=FALSE)
  test_idx  <- (1:length(x))[-(train_idx)]

  train     <- data.frame(x[train_idx],
                          y[train_idx])
  colnames(train) <- c("x", "y")

  test    <- data.frame(x[test_idx],
                          y[test_idx])
  colnames(test) <- c("x", "y")

  return(list(train=train, test=test, train_idx=train_idx))
}

dna.filter <- function(fover){
	#' :detail: removes data labels as Do Not Analyze
	#' :param fover: overcast.filter results
	#' :return: overcast.filter results with DNA points removed
	#' :rtype: list
	sky.len <- length(grep("snsr_sky", names(fover), fixed=TRUE))
	gro.len <- length(grep("snsr_gro", names(fover), fixed=TRUE))
	pw.len 	<- length(grep("pw_loc", names(fover), fixed=TRUE))
	dna.len <- grep("DNA", fover$com, fixed=TRUE)

	if (length(dna.len) > 0){
		for (j in 1:sky.len){
			fover[[ paste("snsr_sky",j,sep="") ]][dna.len] <- NaN
		}
		for (j in 1:gro.len){
			fover[[ paste("snsr_gro",j,sep="") ]][dna.len] <- NaN
		}
		for (j in 1:pw.len){
			fover[[ paste("pw_loc",j,sep="") ]][dna.len] <- NaN
		}
	}
	return(fover)
}

nan.filter <- function(stuff){
	#' :detail: removes nan values from a set of lists
	#' :param list stuff: list of arrays
	#' :return: returns list with filtered data and the indicies with nans
	#' :rtype: list
	nans <- list()
	for (i in 1:length(stuff)){
		if (length(stuff[[i]][[1]]) > length(lengths(stuff[[i]]))){
			for (j in stuff[[i]]){
				nans <- append(nans, grep(NaN, j))
			}
		} else {
			nans <- append(nans, grep(NaN,  stuff[[i]]))
		}
	}
	nans <- unique(unlist(nans))
	if (length(nans) > 0){
		for (i in 1:length(stuff)){
			if (length(stuff[[i]][[1]]) > length(lengths(stuff[[i]]))){
				for (j in 1:length(stuff[[i]])){
					stuff[[i]][[j]] <- stuff[[i]][[j]][-(nans)]
				}
			} else {
				stuff[[i]] <- stuff[[i]][-(nans)]
			}
		}
	}
	return(list(stuff, nans))
}

overcast.filter <- function(col_con, col_date, col_com, pw_name, snsr_name, cloud_bool){
	#' :detail: Filters our data with overcast condition
	#' :param integer col_con: column index for condition labels
	#' :param integer col_date: column index for date stamp
	#' :param integer col_com: column index for comments
	#' :param list pw_name: pw measurement labels
	#' :param list snsr_name: sensor labels
	#' :return: A list of lists containing either clear-sky/overcast data
	#' :rtype: list
	# Initializes the lists to store values
	date_clear	<- snsr_sky		<- snsr_gro		<- pw_loc  <- rh <- time <- dew <- list()
	date_over	<- snsr_skyo	<- snsr_groo	<- pw_loco <- rho <- timeo <- dewo<- list()
	com <- como <- list()
	# Divides the data based on condition (Overcast/Clear Skies)
	for (i in 1:length(t(fname[col_con]))){
		if ("clear sky" %in% fname[i,col_con]){
			date_clear  <- append(date_clear, lapply(fname[[i, as.numeric(col_date)]], as.Date, "%Y-%m-%d" ))
			for (j in 1:length(pw_name)) {
				pw_loc[[ paste("pw_loc", j, sep="")]] 		<- append(x=pw_loc[[ paste("pw_loc", j, sep="")]],  values=fname[i, col_pw[j]])
			}
			for (j in 1:length(snsr_name)) {
				snsr_gro[[ paste("snsr_gro",j,sep="") ]] 	<- append(x=snsr_gro[[ paste("snsr_gro",j,sep="") ]], values=fname[i, snsr_gro_indx[j]])
				snsr_sky[[ paste("snsr_sky",j,sep="") ]] 	<- append(x=snsr_sky[[ paste("snsr_sky",j,sep="") ]], values=fname[i, snsr_sky_indx[j]])
			}
			rh <- append(x=rh, value=fname[i, col_rh[1]])
			com <- append(x=com, value=fname[i, col_com[1]])
			time <- append(x=time, value=fname[i, col_time[1]])
			dew <- append(x=dew, value=fname[i, col_dew[1]])
		}else{
			date_over   <- append(date_over, lapply(fname[[i, as.numeric(col_date)]], as.Date, "%Y-%m-%d" ))
			for (j in 1:length(pw_name)){
				pw_loco[[ paste("pw_loc", j, sep="")]] <- append(x=pw_loco[[ paste("pw_loc", j, sep="")]],  values=fname[i, col_pw[j]])
			}
			for (j in 1:length(snsr_name)) {
				snsr_groo[[ paste("snsr_gro",j,sep="") ]] <- append(x=snsr_groo[[ paste("snsr_gro",j,sep="") ]], values=fname[i, snsr_gro_indx[j]])
				snsr_skyo[[ paste("snsr_sky",j,sep="") ]]<- append(x=snsr_skyo[[ paste("snsr_sky",j,sep="") ]], values=fname[i, snsr_sky_indx[j]])
			}
			rho <- append(x=rho, value=fname[i, col_rh[1]])
			como <- append(x=como, value=fname[i, col_com[1]])
			timeo <- append(x=timeo, value=fname[i, col_time[1]])
			dewo <- append(x=dewo, value=fname[i, col_dew[1]])
		}
	}
	# Adds divided data into list to output from function
	output1 <- list()
	if (cloud_bool){
		output1[["date"]] 	<- date_over
		output1[["time"]] 	<- timeo
		output1[["rh"]] 	<- as.numeric(rho)
		output1[["dew"]] 	<- as.numeric(dewo)
		output1[["com"]] 	<- como
		for(j in 1:length(snsr_name)){
			output1[[ paste("snsr_gro",j,sep="") ]] <- as.numeric(snsr_groo[[ paste("snsr_gro",j,sep="") ]])
		}
		for(j in 1:length(snsr_name)){
			output1[[ paste("snsr_sky",j,sep="") ]] <- as.numeric(snsr_skyo[[ paste("snsr_sky",j,sep="") ]])
		}
		for(j in 1:length(pw_name)){
			output1[[ paste("pw_loc",j,sep="") ]] <- as.numeric(pw_loco[[ paste("pw_loc", j, sep="")]])
		}
	} else {
		output1[["date"]] 	<- date_clear
		output1[["time"]] 	<- time
		output1[["rh"]] 	<- as.numeric(rh)
		output1[["dew"]] 	<- as.numeric(dew)
		output1[["com"]] 	<- com
		for(j in 1:length(snsr_name)){
			output1[[ paste("snsr_gro",j,sep="") ]] <- as.numeric(snsr_gro[[ paste("snsr_gro",j,sep="") ]])
		}
		for(j in 1:length(snsr_name)){
			output1[[ paste("snsr_sky",j,sep="") ]] <- as.numeric(snsr_sky[[ paste("snsr_sky",j,sep="") ]])
		}
		for(j in 1:length(pw_name)){
			output1[[ paste("pw_loc",j,sep="") ]] <- as.numeric(pw_loc[[ paste("pw_loc", j, sep="")]])
		}
	}
	return(output1)
}

