fname  <- read.table("./time.txt", sep="\t")



plot(fname[,1], fname[,2], xlab="Step Count", ylab="Time [s]")