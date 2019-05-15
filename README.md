# Precipitable Water Model
Developed by Vicki Kelsey and Spencer Riley.
## Introduction
### Goal
### Instrumentation
```
Precipitable-Water-Model
|
|--- data/
|   `--- master_data.csv
|
|--- install.sh
|
|--- README.md
|
`--- src/
	|--- archive/
	|   |--- main.py
	|   |--- mrop.py
	|   `--- plots_galore.py
	`--- model.r
```

## Methodology

## Data Format

## Requirements
To satisfy the requirements to execute the script. Run ```install.sh```. 
It will install the system requirements and the R package 
requirements.

```bash
$ bash install.sh
```

## Using the Model

The computational model is enclosed in the script ``model.r``. 
The capabilities of the program include the production of three groups of 
plots:

 1)
 
 2)

 3)
 
There are two primary sets of plots, the first set pulls 
air and ground temperature measurements and the date
and plots the air, ground, and change in the air and ground
temperature as a time series. 
```bash
$ Rscript model.r --help

usage: model.r [-h] [--save] [--opt OPT] [--poster] [--dev] [-o]

optional arguments:
  -h, --help      show this help message and exit
  --save          Saves plots
  --opt OPT       Select plot sets: (m)ain/(p)lots_galore/(o)ther
  --poster        Produces poster plots
  --dev           Development plots
  --data          Produces two columned dataset including mean temp and PW
  -o, --overcast  Shows time series data for days with overcast condition
				  (Used with --opt m)
```
### 'Main' Set Contents
 1) Air Temperature Time Series

 2) Ground Temperature Time Series

 3) Change in Temperature Time Series

The second set plots the 
temperature and precipitable water. The current configuration
of ```model.r``` is set such that there are a source of 
precipitable water data for two locations at two 
different times. 
```bash
$ Rscript model.r --opt m
```
```bash
$ Rscript model.r --opt m --overcast
```

### 'Plots Galore' Set Contents
 1) Individual Location PW and Temperature
 
 2) Locational Average PW and Temperature

 3) Total Mean PW and Temperature
 
 4) Residual for Total Mean PW and Temperature  
```bash
 $ Rscript model.r --opt p
```
### 'Other' Set Contents
 1) Overcast Condition Percentage (Bar)
 
 2) Overcast Condition Percentage (Pie)
```bash
$ Rscript model.r --opt o
```
## R Features
The following sections define and show interesting 
features in the R source code
that exist as a part of the model. To use the following 
code snippets it is important to run the bash script
defined in [Requirements](#precipitable-water-model-requirements). 
### Show and Save Functions
This collection of functions uses the X11 framework 
to produce pop-up windows of the visual outputs of
the R script. The ```show()``` function produces the 
plotting window, and the ```continue_input()``` function
works to keep the window open until the Enter key is
inputted into the terminal. Without the ```continue_input()```
function, the plotting window would automatically open and close, ending the 
script. 
```R
## Allows the plots to stay open
continue_input <- function(){
	cat(bold(yellow("Press Enter to Continue:\n>> ")))
	x <- readLines(con="stdin", 1)
}
## A function that will produce popups through the x11 framework
show <- function(...){
	args <- list(...)
	for (i in args){
		X11(type="cairo", width=n, height=n)
		i()
	}
	continue_input()
}

## A general function that will save plots
save <- function(func, name){
	pdf(name)
	func
	dev.off()
}
```
The ```save()``` function can be used in-joint with the ```show()```
function, as seen in the usage snippet. The resulting output
is a PDF file by the name "cool_plots.pdf" with three pages, one
for each plot unless otherwise specified.
```R
### USAGE
dummy <- function(){
## The test_plots are functions that make plots
	show(test_plot1, test_plot2, test_plot3)
	save(c(test_plot1(), test_plot2(), test_plot3()), "cool_plots")
}
```
### Exponential Regression
```R
## Data
y 	<- as.numeric(ydata)
x 	<- as.numeric(xdata)

## Max & Min values for limits
ymax = max(y, na.rm=TRUE)
ymin = min(y, na.rm=TRUE)

xmin = min(x, na.rm=TRUE)
xmax = max(x, na.rm=TRUE)

## Sequence between the minimum and maximum x value
newx 	<- seq(xmin, xmax, length.out=length(x))

# Non-linear model (exponential)
plot(x,y, col=c("blueviolet"), pch=16,
	xlim=c(xmin, xmax), ylim=c(ymin, ymax))

model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)

q 	<- coef(model)

## Trendline
curve(exp(q[1] +q[2]*x), col="Red", add=TRUE)

## Confidence Interval
confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')

lines(newx, exp(confint[ ,3]), col="blue", lty="dashed")
lines(newx, exp(confint[ ,2]), col="blue", lty="dashed")

## Prediction Interval
predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')

lines(newx, exp(predint[ ,3]), col="magenta", lty="dashed")
lines(newx, exp(predint[ ,2]), col="magenta", lty="dashed")
```
### The Pac-Man Residual plot
```R
## Data
y 	<- as.numeric(ydata)
x 	<- as.numeric(xdata)

## Max & Min values for limits
ymax    <- max(y, na.rm=TRUE)
ymin    <- min(y, na.rm=TRUE)

xmin    <- min(x, na.rm=TRUE)
xmax    <- max(x, na.rm=TRUE)

## Sequence between the minimum and maximum x value
newx 	<- seq(xmin, xmax, length.out=length(x))

# Non-linear model (exponential)
plot(x,y, col=c("blueviolet"), pch=16,
	xlim=c(xmin, xmax), ylim=c(ymin, ymax))

model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)

residual <- abs(resid(model))
t 	<- seq(40, 320, len=length(residual))

rmax 	<- max(as.numeric(residual), na.rm=TRUE)
test    <- polar.plot(residual, t, rp.type="s",labels="",
		radial.lim=c(0, 1),show.grid=TRUE, show.grid.labels=FALSE,
		main="Pac-Man Residual Plot",
		show.radial.grid=FALSE, grid.col="black")

## Alternates Colors for contrast
color1 <- "Yellow"
color2 <- "White"
draw.circle(0, 0, radius=1, col=color1)
draw.circle(0, 0, radius=0.8, col=color2)
draw.circle(0, 0, radius=0.6, col=color1)
draw.circle(0, 0, radius=0.4, col=color2)
draw.circle(0, 0, radius=0.2, col=color1)

## Plots output of residual against an arbitary angle
polar.plot(residual, t, rp.type="s",point.col="blue",
	point.symbols=16, add=TRUE)

## Labeling 
text(c(0.12, 0.3, 0.5, 0.7, .9), 0, c(0.2, 0.4, 0.6, 0.8, 1.0), cex=1)

## Defines region dedicated to labeling
polar.plot(c(0, 1), c(min(t) - 10, min(t) - 10), lwd=1, rp.type="p",
	line.col="black", add=TRUE)
polar.plot(c(0, 1), c(max(t) + 10, max(t) + 10), lwd=1, rp.type="p",
	line.col="black", add=TRUE)
```
