#' @file pmat_analysis.r
#' @author Spencer Riley
#' @brief functions for analysis
#' @docs https://docs.pmat.app
#' @help To get a list of arguments run [Rscript model.r --help]

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
## Pulls the column number of the Relative Humidity
col_rh 		<- grep("RH", colnames(fname))
## Pulls the column number of the non-measurement temperature
col_temp 	<- grep("Temp", colnames(fname))
## Pulls the column number of the Condition
col_con 	<- grep("Condition", colnames(fname))
## Pulls the column number for the comments
col_com 	<- grep("comments", colnames(fname))
## The value for the training fraction
train_frac 	<- config[[as.numeric(length(config) - 1)]]$value
## The value for the threshold of the mean.filter
rel_diff 	<- config[[length(config)]]$value

## Pulls sensor labels and colors from instruments.txt
snsr_name 	<- list(); snsr_color <- snsr_sky_indx <- snsr_gro_indx  	<- unlist(list())
for(i in 1:length(config)){
	if (!(length(config[[i]]$sensor$active) == 0)){
		var 				<- assign(paste("Thermo", i, sep=""), config[[i]]$sensor$name)
		snsr_name 			<- append(snsr_name, toString(var))
		snsr_color 			<- append(snsr_color, paste("#", toString(config[[i]]$sensor$color), sep=""))
		snsr_sky_indx 		<- append(snsr_sky_indx, col_sky[i])
		snsr_gro_indx 		<- append(snsr_gro_indx, col_gro[i])
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
	pw_name <- append(pw_name, name)
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


null.scope <- function(x, y){
		nans <- c()
		if (length(c(grep("NaN", y))) != 0){
			nans <- append(nans, grep("NaN", y))
		}
		if (length(c(grep("NaN", x))) != 0){
			nans <- append(nans, grep("NaN", x))
		}
		if (!is.null(nans)){
			x <- x[-(nans)]
			y <- y[-(nans)]
		} else {
			x <- x; y <- y;
		}
	# 	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	# x <- x[-(nans)]; y <- y[-(nans)]
	return(list("x"=x, "y"=y))
}

#' @title mean.filter
#' @description filters the data based on the relative difference of the mean pw
#' @param pw precipitable water data
#' @param avg the average values for pw
#' @param percent the threshold in percent
#' @return A sky temperature time series plot
#' @export
mean.filter <- function(pw, avg, percent){
    storage <- bad <- good <- list()
    for (i in 1:length(pw)){
        out <- append(x=storage, values=Map("/",Map("-",unlist(pw[i]),avg), avg))
        for (j in 1:length(out)){
            if (is.na(out[j])){
                next
            }else if (abs(as.numeric(out[j])) >= percent/100){
                bad[[ paste("bad",i,sep="") ]] 	<- append(x=bad[[ paste("bad",i,sep="") ]], values=j)
            }else{
                good[[ paste("good",i,sep="") ]] <- append(x=good[[ paste("good",i,sep="") ]], values=j)
            }
        }
    }
    bad <- sort(unique(Reduce(c, bad)))
    good <- sort(unique(Reduce(c, good)))
    good <- good[!(good %in% bad)]
    return(good)
}

#' @title data.parition
#' @description splits the data into a training/testing set
#' @param x domain of the data
#' @param y range of the data
#' @param train_size fraction of the data in the testing set [Default is 0.7]
#' @param rand_state the seed for the random generated partition
#' @return A sky temperature time series plot
#' @export
data.partition <- function(x,y, train_size=0.7, rand_state=sample(1:2^15, 1)){
	x <- null.scope(x,y)$x
	y <- null.scope(x,y)$y
  set.seed(rand_state)
		# Finds and removes NaNed values from the dataset
  train_idx <- sample(1:length(x), trunc(length(x)*train_size), replace=FALSE)
  test_idx  <- (1:length(x))[-(train_idx)]

  train     <- data.frame(x[train_idx],
                          y[train_idx])
  colnames(train) <- c("x", "y")

  test    <- data.frame(x[test_idx],
                          y[test_idx])
  colnames(test) <- c("x", "y")

  return(list(train=train, test=test, train_idx=train_idx, seed=rand_state))
}

#' @title overcast.filter
#' @description Filters our data with overcast condition
#' @param col_con column number for condition
#' @param col_date column number for date stamp
#' @param col_com column number for comments
#' @param pw_name pw measurement labels
#' @param sensr_name sensor labels
#' @return A sky temperature time series plot
#' @export
overcast.filter <- function(col_con, col_date, col_com, pw_name, snsr_name){
	# Initializes the lists to store values
	date_clear	<- snsr_sky		<- snsr_gro		<- pw_loc  <- rh  <- list()
	date_over	<- snsr_skyo	<- snsr_groo	<- pw_loco <- rho <- list()
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
		}else{
			date_over   <- append(date_over, lapply(fname[[i, as.numeric(col_date)]], as.Date, "%Y-%m-%d" ))
			for (j in 1:length(pw_name)){
				pw_loco[[ paste("pw_loco", j, sep="")]] <- append(x=pw_loco[[ paste("pw_loco", j, sep="")]],  values=fname[i, col_pw[j]])
			}
			for (j in 1:length(snsr_name)) {
				snsr_groo[[ paste("snsr_groo",j,sep="") ]] <- append(x=snsr_groo[[ paste("snsr_groo",j,sep="") ]], values=fname[i, snsr_gro_indx[j]])
				snsr_skyo[[ paste("snsr_skyo",j,sep="") ]]<- append(x=snsr_skyo[[ paste("snsr_skyo",j,sep="") ]], values=fname[i, snsr_sky_indx[j]])
			}
			rho <- append(x=rho, value=fname[i, col_rh[1]])
			como <- append(x=como, value=fname[i, col_com[1]])
		}
	}
	# Adds divided data into list to output from function
	output1 <- list(clear_date=date_clear, over_date=date_over, rh=rh, rho=rho,com=com)
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_gro"=snsr_gro[[ paste("snsr_gro",j,sep="") ]]))
	}
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_sky"=snsr_sky[[ paste("snsr_sky",j,sep="") ]]))
	}
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_sky"=snsr_skyo[[ paste("snsr_skyo",j,sep="") ]]))
	}
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_gro"=snsr_groo[[ paste("snsr_groo",j,sep="") ]]))
	}
	for(j in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("clear_pw"=pw_loc[[ paste("pw_loc", j, sep="")]]))
	}
	for(j in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("over_pw"=pw_loco[[ paste("pw_loco", j, sep="")]]))
	}
	return(output1)
}
