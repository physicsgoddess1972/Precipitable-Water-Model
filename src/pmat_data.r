data1 <- function(overcast, dir){
    if (overcast){
	# Pulls the data
      avg_temp	<- as.numeric(unlist(overcast.results$snsr_sky_calc))
      avg_pw 	<- overcast.results$avg
      date      <- overcast.results$date
      class(date) <- "Date"
	# Pulls the data
      norm  		<- data.frame(list(x=date, y1=avg_temp, y2=avg_pw))
	# Removes the NaN data
      norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
      norm 		<- norm[-c(which(avg_temp %in% NaN)), ]
	# Adds data to a data frame with column names
      data 		<- data.frame(list(date=c(norm$x), avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
      colnames(data) <- c("date", "avg_temp", "avg_pw")
	# Writes the data to a csv
      write.csv(data, file=sprintf("../data/data_overcast.csv"), row.names=FALSE)
      cat(green(sprintf("Data sent to data/data_overcast.csv\n")))
    }else{
	# Pulls the data
        avg_temp	<- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
        avg_pw 		<- clear_sky.results$avg
        date        <- unlist(clear_sky.results$date)
        class(date) <- "Date"
	# Pulls the data
        norm  		<- data.frame(list(x=date, y1=avg_temp, y2=avg_pw))
	# Removes the NaN data
        norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
        norm 		<- norm[-c(which(avg_temp %in% NaN)), ]
       data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
        colnames(data) <- c("date", "avg_temp", "avg_pw")
	# Writes the data to a csv
        write.csv(data, file=sprintf("%sdata_clear.csv", dir), row.names=FALSE)
        cat(green(sprintf("Data sent to %sdata_clear.csv\n", dir)))

    }
}

data2 <- function(dir){
      ml_pw <- ml_pw_avg <- ml_temp <- ml_temp_avg <- ml_rh <- list()
      ## Average PW
      for(a in 1:length(col_pw)){
          ml_pw[[ paste("ml_pw", a, sep="") ]] <- as.numeric(unlist(fname[col_pw[a]]))
      }
      for(a in ml_pw){
          for(b in 1:(length(unlist(ml_pw))/length(ml_pw))){
              ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]] <- append(x=ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]], value=na.omit(c(a[b])))
          }
      }
      for(a in 1:(length(unlist(ml_pw))/length(ml_pw))){
          ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]] <- mean(ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]])
      }
      # Average Temperature
      for(a in 1:length(col_sky)){
          ml_temp[[ paste("ml_temp", a, sep="") ]] <- as.numeric(unlist(fname[col_sky[a]]))
          ml_temp[[ paste("ml_temp", a, sep="") ]] <- replace(ml_temp[[ paste("ml_temp", a, sep="") ]], ml_temp[[ paste("ml_temp", a, sep="") ]] == "-Inf", NaN)
      }

      for(a in ml_temp){
          for(b in 1:(length(unlist(ml_temp))/length(ml_temp))){
              ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]] <- append(x=ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]], value=na.omit(c(a[b])))
          }
      }
      for(a in 1:(length(unlist(ml_temp))/length(ml_temp))){
          ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]] <- mean(ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]])
      }
      ## Relative Humidity
      for(a in 1:length(col_rh)){
          ml_rh[[ paste("ml_rh", a, sep="") ]] <- as.numeric(unlist(fname[col_rh[a]]))
      }
  # Pulls the data
      avg_temp	<- as.numeric(unlist(ml_temp_avg))
      avg_pw 		<- as.numeric(unlist(ml_pw_avg))
      avg_rh 		<- as.numeric(unlist(ml_rh))
      date 		<- as.Date(fname[ ,col_date], "%m/%d/%Y")
      cond 		<- fname[,col_con]
  # Pulls the data
      norm  		<- na.omit(data.frame(list(x=date, y1=avg_temp, y2=avg_pw, y3=avg_rh, c=cond)))
      data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2), avg_rh=c(norm$y3), cond=c(norm$c)))
      colnames(data) <- c("date", "avg_temp", "avg_pw", "avg_rh", "condition")
  # Writes the data to a csv
      write.csv(data, file=sprintf("%sml_data.csv", dir), row.names=FALSE)
      cat(green(sprintf("Data sent to %sml_data.csv\n", dir)))
}