library(crayon)

fname       <- read.csv(file="../../Precipitable-Water-Model/data/master_data.csv", sep=",")

## Filters out data with overcast condition
overcast_filter <- function(){
# Initializes the lists to store values
	date_clear	<- overcast1 	<- overcast2 	<- overcast3 	<- list()
	overcast4 	<- overcast5 	<- overcast6 	<- overcast7 	<- list()
	overcast8 	<- overcast9 	<- overcast10 	<- list()

	date_over 	<- overcast1o 	<- overcast2o 	<- overcast3o	<- list()
	overcast4o 	<- overcast5o 	<- overcast6o	<- overcast7o	<- list()
	overcast8o	<- overcast9o 	<- overcast10o	<- list()

# Divides the data based on condition (Overcast/Clear Skies)
	for (j in 1:length(t(fname[12]))){
		if (!"overcast" %in% fname[j,12]){
			date_clear  <- append(date_clear, as.Date(fname[j, 1], "%m/%d/%Y"))

			overcast1 	<- append(overcast1, fname[j, 7])
			overcast2 	<- append(overcast2, fname[j, 8])
			overcast3 	<- append(overcast3, fname[j, 9])
			overcast4 	<- append(overcast4, fname[j, 10])
			overcast5 	<- append(overcast5, fname[j, 11])
			overcast6   <- append(overcast6, fname[j, 2])
			overcast7   <- append(overcast7, fname[j, 3])
			overcast8   <- append(overcast8, fname[j, 4])
			overcast9   <- append(overcast9, fname[j, 5])
			overcast10  <- append(overcast10, fname[j, 6])
		}
		else{
			date_over   <- append(date_over, as.Date(fname[j, 1], "%m/%d/%Y"))

			overcast1o 	<- append(overcast1o, fname[j, 7])
			overcast2o 	<- append(overcast2o, fname[j, 8])
			overcast3o 	<- append(overcast3o, fname[j, 9])
			overcast4o 	<- append(overcast4o, fname[j, 10])
			overcast5o 	<- append(overcast5o, fname[j, 11])
			overcast6o 	<- append(overcast6o, fname[j, 2])
			overcast7o 	<- append(overcast7o, fname[j, 3])
			overcast8o 	<- append(overcast8o, fname[j, 4])
			overcast9o 	<- append(overcast9o, fname[j, 5])
			overcast10o <- append(overcast10o, fname[j, 6])
		}
	}
	output <- list("y1"=overcast1,"y2"=overcast2, "y3"=overcast3,"y4"=overcast4,
					"y5"=overcast5, "y6"=overcast6,"y7"=overcast7, "y8"=overcast8,
					"y9"=overcast9, "y10"=overcast10,
					"y1o"=overcast1o, "y2o"=overcast2o, "y3o"=overcast3o,
					"y4o"=overcast3o, "y5o"=overcast5o,
					"y6o"=overcast6o, "y7o"=overcast7o, "y8o"=overcast8o,
					"y9o"=overcast9o, "y10o"=overcast10o,"y0"=date_clear, "y0o"=date_over)
	return (output)
}
## Pushes returned values to the variable overcast
overcast <- overcast_filter()

pw_abq12    <- as.numeric(array(t(overcast$y2)))    	# PW for ABQ @ 12Z
pw_abq00	<- as.numeric(array(t(overcast$y3)))    	# PW for ABQ @ 00Z
pw_epz12    <- as.numeric(array(t(overcast$y4)))    	# PW for EPZ @ 12Z
pw_epz00    <- as.numeric(array(t(overcast$y5)))    	# PW for EPZ @ 00Z

atemp_am    <- as.numeric(array(t(overcast$y1)))    	# Air Temperature from AMES

abq 	<- (as.numeric(pw_abq12) + as.numeric(pw_abq00))/2 # ABQ average
epz 	<- (as.numeric(pw_epz12) + as.numeric(pw_epz00))/2 # EPZ average
avg 	<- (abq + epz)/2 								# Super Average

x <- atemp_am
y <- log(avg, base=exp(1))

xmin <- min(x, na.rm=TRUE)
xmax <- max(x, na.rm=TRUE)
ymax <- max(y, na.rm=TRUE)
ymin <- min(y, na.rm=TRUE)
newx 	<- seq(xmin, xmax, length.out=length(x))


model.0 <- lm(y~x, data=data.frame(x,y))
start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
model 	<- nls(y~a+b*x, data=data.frame(x=x, y=y), start=start)

confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')

summary(model)

x11(type="cairo")
plot(x,y, col=c("blueviolet"), pch=16, xlim=c(xmin,xmax),
    ylim=c(ymin, ymax))

curve(coef(model)[1]+coef(model)[2]*x, col="Red", add=TRUE)
# Confidence Interval
	lines(newx, confint[ ,3], col="blue", lty="dashed")
	lines(newx, confint[ ,2], col="blue", lty="dashed")
# Prediction Interval
	lines(newx, predint[ ,3], col="magenta", lty="dashed")
	lines(newx, predint[ ,2], col="magenta", lty="dashed")

cat(bold(yellow("Slam Enter to Continue:\n>> "))); w <- readLines(con="stdin", 1)

x11(type="cairo")
plot(x,exp(y), col=c("blueviolet"), pch=16, xlim=c(xmin,xmax),
    ylim=c(exp(ymin), exp(ymax)))

curve(exp(coef(model)[1]+coef(model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
	lines(newx, exp(confint[ ,3]), col="blue", lty="dashed")
	lines(newx, exp(confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
	lines(newx, exp(predint[ ,3]), col="magenta", lty="dashed")
	lines(newx, exp(predint[ ,2]), col="magenta", lty="dashed")

cat(bold(yellow("Slam Enter to Continue:\n>> "))); w <- readLines(con="stdin", 1)
