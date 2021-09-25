#' @title inf_counter
#' @description identifies the -Inf values
#' @param bool decides if -Inf is not replaced with NaN
#' @param snsr_data the dataset
#' @param label the identifer for the dataset (e.g. sky, gro, skyo, groo)
#' @return data set that replaces all -Infs for NaN (If bool == FALSE).
#' @export
inf_counter <- function(bool, snsr_data, label){
    output <- list()
    for (i in seq(1, length(snsr_data))){
        if (bool == FALSE){
            if ('-Inf' %in% snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]]) {
                snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]] <- replace(snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]], snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]] == "-Inf", NaN)
            }
        }
        output <- append(output, values=list(snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]]))
    }
    return(output)
}

#' @title exp.regression
#' @description Function includes all of the stuff to generate the exponential regression model with intervals
#' @param x the domain of the dataset
#' @param y the range of the dataset
#' @return A list of stat stuff
#' @export
exp.regression 	<- function(x,y){
	# Finds and removes NaNed values from the dataset
	# nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	# x <- x[-(nans)]; y <- y[-(nans)]
	# Porblem seems to be nan values are still being counted in the resid(model.0)
	if (args$overcast){
		pw_loc 	<- overcast.results$pw_loc
		avg 	<- overcast.results$avg
	} else {
		pw_loc 	<- clear_sky.results$pw_loc
		avg 	<- clear_sky.results$avg
	}
	data_indx <- mean.filter(pw_loc, avg)
	data_sep <- data.partition(x[data_indx], y[data_indx])
	# data_sep <- data.partition(x, y)
	train <- data_sep$train
	test <- data_sep$test 
	print(train$x)
	# creates a uniform sequence of numbers that fit within the limits of x
	xmin 	<- min(train$x, na.rm=TRUE)
	xmax 	<- max(train$x, na.rm=TRUE)
	newx 	<- seq(xmin, xmax, length.out=length(train$x))
	# Non-linear model (exponential)
	## Initial values are in fact the converged values
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x=train$x, y=train$y))
	# print(as.numeric(unlist(resid(model.0))))
	print(length(resid(model.0)))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(log(y, base=exp(1))~a+x*b, data=data.frame(x=train$x, y=train$y), start=start)
	# Intervals (confidence/prediction)
	confint <- predict(model.0, newdata=data.frame(q=newx), interval='confidence')
	predint <- predict(model.0, newdata=data.frame(q=newx), interval='prediction')
	# Coefficient of determination
	r2		<- summary(model.0)$r.squared
    # estimate from regression
	est     <- exp(coef(model)[1]+coef(model)[2]*test$x)
	# accuracy of model
	acc     <- sqrt((1/length(test$x))*(sum((est-test$y)^2)/length(test$x)))
    # Residual Standard Deiviation
	S       <- sqrt(sum((est-test$y)^2)/(length(test$x) - 2))
	# Root Square Mean Error
	rsme    <- sqrt(sum((est-test$y)^2)/length(test$x))
	# Function outputs
	output 	<- list("x"=train$x, 
					"y"=train$y, 
					"newx"=newx, 
					"xmin"=xmin, 
					"xmax"=xmax, 
					"model.0"=model.0,
					"model"=model, 
					"confint"=confint, 
					"predint"=predint, 
					"R2"=r2,
					'est'=est, 
					'acc'=acc, 
					'S'=S, 
					'rsme'=rsme, 
					"seed"=def_seed)
	return (output)
}

#' @title clear_sky.analysis
#' @description Computes all averages for plotting for clear sky labeled data
#' @param overcast results of the overcast.filter function
#' @export
clear_sky.analysis <- function(overcast){
## Pulls date from filter function
	clear_date  <- overcast$clear_date	# Date
	comments    <- overcast$com
## Pulls relative humidity from filter function
	clear_rh <- as.numeric(overcast$rh)
## Initialize empty lists
	snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- tmp_avg <- list()
## Adds PW measurements for clear sky to list
	for (i in 1:length(pw_name)){
		pw_loc[[ paste("pw_loc", i, sep="")]]	 <- as.numeric(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[1]+i-1]))
	}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
	for (i in 1:length(snsr_name)){
		snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_del[[ paste("snsr_del",i,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+i-1])) - as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+i-1]))
	}

	out_sky <- inf_counter(FALSE, snsr_sky, 'sky')
	for (i in 1:length(snsr_sky)){
		snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- out_sky[[i]]
	}

	out_gro <- inf_counter(FALSE, snsr_gro, 'gro')
	for (i in 1:length(snsr_gro)){
		snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- out_gro[[i]]
	}
	for (i in 1:length(clear_date)) {
		if (grepl("This datapoint has been omitted from the final analysis; refer to documentation on how to handle this day", comments[i], fixed=TRUE)){
			for (j in 1:length(snsr_name)){
				snsr_sky[[ paste("snsr_sky",j,sep="") ]][i] <- "NaN"
				snsr_gro[[ paste("snsr_gro",j,sep="") ]][i] <- "NaN"
			}
		}
	}
	for (i in 1:length(snsr_sky)){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=as.numeric(snsr_sky[[i]][j]))
		}
	}	
	print(snsr_sky_calc[[100]])
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(na.omit(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]]))
	}
## Takes locational average of the precipitable water measurements
	for (i in 1:length(col_pwpl)){
		tmp <- unlist(col_pwpl[i])
		for (j in col_pwpl[i]){
			loc_avg[[ paste("loc_avg",i,sep="") ]] <-
				array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- loc_avg[i]
		loc_avg[[ paste("loc_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwpl)
	}
	for (i in 1:length(col_pwtm)){
		tmp <- unlist(col_pwtm[i])
		for (j in col_pwtm[i]){
			tmp_avg[[ paste("tmp_avg",i,sep="") ]] <-
				array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- tmp_avg[i]
		tmp_avg[[ paste("tmp_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwtm)
	}
## Takes super average of the precipitable water measurements
	avg 		<-  Reduce("+", pw_loc)/length(pw_loc)
	return(list("avg"=avg,
				"date"=clear_date,
				"snsr_sky"=snsr_sky,
				"snsr_gro"=snsr_gro,
				"snsr_del"=snsr_del,
				"rh"=clear_rh,
				"comments"=comments,
				"pw_loc"=pw_loc,
				"loc_avg"=loc_avg,
				"tmp_avg"=tmp_avg,
				"snsr_sky_calc"=snsr_sky_calc))
}

#' @title overcast.analysis
#' @description Computes all averages for plotting for overcast labeled data
#' @param overcast results of the overcast.filter function
#' @export
overcast.analysis <- function(overcast){
## Pulls date from filter function (overcast)
	over_date  	<- overcast$over_date
## Pulls relative humidity from filter function
	over_rh <- as.numeric(overcast$rho)
	comments    <- overcast$como
# Initialize empty lists
	snsr_delo  	<- snsr_skyo <- snsr_groo <- pw_loco <- loc_avgo <- snsr_sky_calco <- tmp_avgo <- list()
## Adds PW measurements for overcast to list
	for (i in 1:length(pw_name)){
		pw_loco[[ paste("pw_loco", i, sep="")]] 	<- as.numeric(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[1]+i-1]))
	}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
	for (i in 1:length(snsr_name)){
		snsr_skyo[[ paste("snsr_skyo",i,sep="") ]] <- as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_groo[[ paste("snsr_groo",i,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_delo[[ paste("snsr_delo",i,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+i-1])) - as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+i-1]))
	}

	out_skyo <- inf_counter(FALSE, snsr_skyo, 'skyo')
	for (i in seq(1, length(snsr_skyo))){
		snsr_skyo[[ paste("snsr_skyo",i,sep="") ]] <- out_skyo[[i]]
	}

	out_groo <- inf_counter(FALSE, snsr_groo, 'groo')
	for (i in seq(1, length(snsr_groo))){
		snsr_groo[[ paste("snsr_groo",i,sep="") ]] <- out_groo[[i]]
	}
	for (i in seq(from = 1,to = length(over_date))) {
		if (grepl("This datapoint has been omitted from the final analysis; refer to documentation on how to handle this day", comments[i], fixed=TRUE)){
			for (j in length(snsr_skyo)){
				snsr_skyo[[ paste("snsr_skyo",j,sep="") ]][i] <- "-Inf"
			}
			for (j in length(snsr_gro)){
				snsr_groo[[ paste("snsr_groo",j,sep="") ]][i] <- "Inf"
			}
		}
	}
## Takes average of available sky temperature measurements
# Removes all NaN values from daily lists
	for (i in snsr_skyo){
		for (j in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
			snsr_sky_calco[[ paste("snsr_sky_calco",j,sep="") ]] <-
				append(x=snsr_sky_calco[[ paste("snsr_sky_calco", j, sep="")]], values=na.omit(c(i[j])))
		}
	}
# Takes averages of each list
	for (i in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
		snsr_sky_calco[[ paste("snsr_sky_calco",i,sep="") ]] <- mean(snsr_sky_calco[[ paste("snsr_sky_calco",i,sep="") ]])
	}
## Takes locational average of the precipitable water measurements
	for (i in 1:length(col_pwpl)){
		tmp <- unlist(col_pwpl[i])
		for (j in col_pwpl[i]){
			loc_avgo[[ paste("loc_avgo",i,sep="") ]] <-
				array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- loc_avgo[i]
		loc_avgo[[ paste("loc_avgo",i,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwpl)
	}
## Takes temporal average of the precipitable water measurements
	for (i in 1:length(col_pwtm)){
		tmp <- unlist(col_pwtm[i])
		for (j in col_pwtm[i]){
			tmp_avgo[[ paste("tmp_avgo",i,sep="") ]] <-
				array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- tmp_avgo[i]
		tmp_avgo[[ paste("tmp_avgo",i,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwtm)
	}
## Takes super average of the precipitable water measurements
	avgo 		<-  Reduce("+", pw_loco)/length(pw_loco)
	return(list("avg"=avgo,
				"date"=over_date,
				"snsr_sky"=snsr_skyo,
				"snsr_gro"=snsr_groo,
				"snsr_del"=snsr_delo,
				"rh"=over_rh,
				"pw_loc"=pw_loco,
				"loc_avg"=loc_avgo,
				"tmp_avg"=tmp_avgo,
				"snsr_sky_calc"=snsr_sky_calco))
}
