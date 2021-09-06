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
  set.seed(rand_state)
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
	date_clear	<- snsr_sky		<- snsr_gro		<- pw_loc  <- rh 	<- list()
	date_over		<- snsr_skyo	<- snsr_groo	<- pw_loco <- rho <- list()
	com <- como <- list()
	# Divides the data based on condition (Overcast/Clear Skies)
	for (i in 1:length(t(fname[col_con]))){
		if ("clear sky" %in% fname[i,col_con]){
			date_clear  <- append(date_clear, lapply(fname[[i, as.numeric(col_date)]], as.Date, "%m/%d/%Y" ))
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
			date_over   <- append(date_over, lapply(fname[[i, as.numeric(col_date)]], as.Date, "%m/%d/%Y" ))
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
