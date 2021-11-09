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
exp.regression 	<- function(x,y,z,t){
	# Finds and removes NaNed values from the dataset
	nans <- c(grep(NaN, y)); nans <- append(nans, grep(NaN, x))
	x <- x[-(nans)]; y <- y[-(nans)];

	# creates a uniform sequence of numbers that fit within the limits of x
	if (t != 1){
		for (i in 1:length(z)){
			z[[ paste("pw_loc",i,sep="") ]] <- z[[ paste("pw_loc",i,sep="") ]][-(nans)]
		}
		data_indx <- mean.filter(z, y, rel_diff)
		data_sep <- data.partition(x[data_indx], y[data_indx], train_frac)
		# data_sep <- data.partition(x, y)
		train <- data_sep$train
		test <- data_sep$test
		#$print(test)
		x <- train$x
		y <- train$y

		tx <- test$x
		ty <- test$y

	} else {
		tx <- x
		ty <- y
	}

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
	r2		<- summary(model.0)$r.squared
    # estimate from regression
	est <- exp(coef(model)[1]+coef(model)[2]*tx)
    # Residual Standard Deiviation
	S       <- sqrt(sum((est-ty)^2)/(length(tx) - 2))
	# Root Square Mean Error
	rsme    <- sqrt(sum((est-ty)^2)/length(tx))
	# Function outputs
	output 	<- list("x"=x, "y"=y, "newx"=newx, "xmin"=xmin, "xmax"=xmax, "model.0"=model.0,
					"model"=model, "confint"=confint, "predint"=predint, "R2"=r2,
					'est'=est,'S'=S, 'rsme'=rsme)
	return (output)
}

#' @title sky.analysis
#' @description Computes all analytics
#' @param overcast results of the overcast.filter function
#' @export
sky.analysis <- function(overcast){
## Pulls date from filter function
	date  		<- overcast$date	# Date
	comments    <- overcast$com
## Pulls relative humidity from filter function
	rh <- as.numeric(overcast$rh)
## Initialize empty lists
	snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- tmp_avg <- list()
## Adds PW measurements for clear sky to list

	for (i in 1:length(pw_name)){
		pw_loc[[ paste("pw_loc", i, sep="")]]	 <- as.numeric(unlist(overcast[grep("pw", names(overcast), fixed=TRUE)[1]+i-1]))
	}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
	for (i in 1:length(snsr_name)){
		snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- as.numeric(unlist(overcast[grep("gro", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- as.numeric(unlist(overcast[grep("sky", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_del[[ paste("snsr_del",i,sep="") ]] <- as.numeric(unlist(overcast[grep("gro", names(overcast), fixed=TRUE)[1]+i-1])) - as.numeric(unlist(overcast[grep("sky", names(overcast), fixed=TRUE)[1]+i-1]))
	}

	for (i in 1:length(date)) {
		if (grepl("DNA", comments[i], fixed=TRUE)){
			for (j in 1:length(snsr_sky)){
				snsr_sky[[ paste("snsr_sky",j,sep="") ]][i] <- NaN
			}
			for (j in 1:length(snsr_gro)){
				snsr_gro[[ paste("snsr_gro",j,sep="") ]][i] <- NaN
			}
		}
	}
	raw_snsr_sky <- snsr_sky
	raw_snsr_gro <- snsr_gro
	out_sky <- inf_counter(FALSE, snsr_sky, 'sky')
	for (i in seq(1, length(snsr_sky))){
		snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- out_sky[[i]]
	}

	out_gro <- inf_counter(FALSE, snsr_gro, 'gro')
	for (i in seq(1, length(snsr_gro))){
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
		for (j in col_pwpl[i]){
			loc_avg[[ paste("loc_avg",i,sep="") ]] <-
				array(overcast[grep("pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- loc_avg[i]
		loc_avg[[ paste("loc_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwpl)
	}
	for (i in 1:length(col_pwtm)){
		for (j in col_pwtm[i]){
			tmp_avg[[ paste("tmp_avg",i,sep="") ]] <-
				array(overcast[grep("pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- tmp_avg[i]
		tmp_avg[[ paste("tmp_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwtm)
	}
## Takes super average of the precipitable water measurements
	avg 		<-  Reduce("+", pw_loc)/length(pw_loc)
	return(list("avg"=avg,
				"date"=date,
				"snsr_sky"=snsr_sky,
				"snsr_gro"=snsr_gro,
				"snsr_del"=snsr_del,
				"rh"=rh,
				"comments"=comments,
				"pw_loc"=pw_loc,
				"loc_avg"=loc_avg,
				"tmp_avg"=tmp_avg,
				"raw_sky"=raw_snsr_sky,
				"raw_gro"=raw_snsr_gro,
				"snsr_sky_calc"=snsr_sky_calc))
}


iterative.analysis <- function(){
  out <- resids <- list()
  for (i in 1:step){
      if (length(config[[length(config)]]$seed) > 0){
      	def_seed 	<- config[[length(config)]]$seed
      } else {
          def_seed 	<- sample(1:2^15, 1)
      }
      set.seed(def_seed)
      if(args$overcast){
        exp_reg <- exp.regression(as.numeric(unlist(overcast.results$snsr_sky_calc)),
                                  overcast.results$avg,
                                  overcast.results$pw_loc, train_frac)
      }else{
        exp_reg <- exp.regression(as.numeric(unlist(clear_sky.results$snsr_sky_calc)),
                                  clear_sky.results$avg,
                                  clear_sky.results$pw_loc, train_frac)
      }
      out 	<- append(out, data3(exp_reg, i, def_seed))
      resids 	<- append(resids, resid(exp_reg$model.0))

    }
  write_yaml(out, paste(args$dir,"_output.yml", sep=""))
  coeff_a <- coeff_b <- rstd <- list()
  for (i in 1:length(out)){
      yml_anly <- read_yaml(text=out[[i]])
      coeff_a <- append(coeff_a, yml_anly$analysis$coeff$A)
      coeff_b <- append(coeff_b, yml_anly$analysis$coeff$B)
      rstd 	<- append(rstd, yml_anly$analysis$rstd)
  }
	A		<- Reduce("+", coeff_a)/length(coeff_a)
	B		<- Reduce("+", coeff_b)/length(coeff_b)
	S       <- Reduce("+", rstd)/length(rstd)
	return(list("A"=A,"B"=B,"S"=S))
}