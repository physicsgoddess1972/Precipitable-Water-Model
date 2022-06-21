#' @file pmat_analysis.r
#' @author Spencer Riley
#' @brief functions for analysis
#' @docs https://docs.pmat.app
#' @help To get a list of arguments run [Rscript model.r --help]

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

	# creates a uniform sequence of numbers that fit within the limits of x
	xmin 	<- min(x, na.rm=TRUE)
	xmax 	<- max(x, na.rm=TRUE)
	newx 	<- seq(xmin, xmax, length.out=length(x))
	# Non-linear model (exponential)
	## Initial values are in fact the converged values
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x=x, y=y))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(log(y, base=exp(1))~a+x*b, data=data.frame(x=x, y=y), start=start)
	# Intervals (confidence/prediction)
	confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
	predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')
	# Coefficient of determination
	r2		<- summary(model.0)$r.squared
    # estimate from regression
	est     <- exp(coef(model)[1]+coef(model)[2]*x)
	# accuracy of model
	acc     <- sqrt((1/length(x))*(sum((est-y)^2)/length(x)))
    # Residual Standard Deiviation
	S       <- sqrt(sum((est-y)^2)/(length(x) - 2))
	# Root Square Mean Error
	rsme    <- sqrt(sum((est-y)^2)/length(x))
	# Function outputs
	output 	<- list("x"=x, "y"=y, "newx"=newx, "xmin"=xmin, "xmax"=xmax, "model.0"=model.0,
					"model"=model, "confint"=confint, "predint"=predint, "R2"=r2,
					'est'=est, 'acc'=acc, 'S'=S, 'rsme'=rsme)
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

	for (i in snsr_sky){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
		}
	}
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
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
	wt_avg 	<- ((3/4) * (Reduce("+", c(pw_loc[1],pw_loc[2])))) + ((1/4) * (Reduce("+", c(pw_loc[3],pw_loc[4]))))#
	avg 	<- Reduce("+", pw_loc)/length(pw_loc)#
	return(list("avg"=avg,
				"wt_avg"=wt_avg,
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

