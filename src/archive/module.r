library(crayon)
# Imports data from master_data.csv
fname       <- read.csv(file="./data.csv", sep=" ")

datax   <- array(t(fname[1]))
datay   <- array(t(fname[2]))
datay 	<- log(datay, base=exp(1))

xmin 	<- min(datax, na.rm=TRUE)
xmax	<- max(datax, na.rm=TRUE)

n       <- 5

continue_input <- function(){
	cat(bold(yellow("Press any Key to Continue:\n>> ")))
	x <- readLines(con="stdin", 1)
	cat(cyan(">>> Program complete <<<\n"))
}

## Super Average Plot with exponential fit
X11(type="cairo", width=n, height=n)
# Non-linear model (exponential)
newx 	<- seq(xmin, xmax, length.out=length(datay))
temp 	<- data.frame(y=datay, x=datax)

model.0 <- lm(datay~datax, data=as.data.frame(fname))
start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
model	<- nls(y~a+b*x, data=temp, start=start)

#print(summary(model))
p 		<- coef(model)
print(p)

plot(datax,exp(datay), col=c("blueviolet"), pch=16,
	xlim=c(xmin, xmax),
	xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
	main="Correlation between Mean\nPrecipitable Water and Temperature")

curve(exp(p[1]+p[2]*x),add=TRUE, col="Green")
legend("topleft",
		legend=parse(text=sprintf('%.2f*e^{%.3f*x}', exp(p[1]),p[2])),
		,col=c("Green"), pch="-")

## Residual Plot
# X11(type="cairo", width=n, height=n)
# residual = resid(model)
# plot(residual, col=c("royalblue"), pch=16,
#     xlab="Zenith Sky Temperature [C]", ylab=expression(sigma),
#     main="Residual Plot for the Mean Precipitable\nWater and Temperature")

continue_input()
