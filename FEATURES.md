<a id="top"></a>

<div id="r-feat">
<div class="collapsible">
<div class="collapsible-header">
	<h2>R Features</h2>
</div>
<div class="panel">
The following sections define and show interesting 
features in the R source code
that exist as a part of the model. To use the following 
code snippets it is important to run the bash script
defined in <a href="#require">Requirements</a>. 
<br><br>
In a more general sense, the features included in the computational model are related to
methods of generalized data input. This is significant because it allows flexibility in the 
number of sensors and precipitable water measurements that can be used. In other words,
two distinct groups, one using three infrared sensors and the other using one hundred sensors, can use
the same model with zero changes to the source code.
</div></div>

<div class="collapsible">
<div class="panel">
<h2> Show and Save Functions </h2>
This collection of functions uses the X11 framework 
to produce pop-up windows of the visual outputs of
the R script. The <code>show()</code> function produces the 
plotting window, and the <code>continue_input()</code> function
works to keep the window open until the Enter key is
inputted into the terminal. Without the <code>continue_input()</code>
function, the plotting window would automatically open and close, ending the 
script. 
<pre lang="R" translate="no" dir="ltr">
<code>
<comment>## Allows the plots to stay open</comment>
continue_input <- function(){
	cat(bold(yellow(<str>"Press Enter to Continue:\n>> "</str>)))
	x <- readLines(con=<str>"stdin"</str>, <num>1</num>)
}
<comment>## A function that will produce popups through the x11 framework</comment>
show <- function(...){
	args <- list(...)
	for (i in args){
		X11(type=<str>"cairo"</str>, width=n, height=n)
		i()
	}
	continue_input()
}
<comment>## A general function that will save plots</comment>
save <- function(func, name){
	pdf(name)
	func
	dev.off()
}

</code>
</pre>
The <code>save()</code> function can be used in-joint with the <code>show()</code>
function, as seen in the usage snippet. The resulting output
is a PDF file by the name "cool_plots.pdf" with three pages, one
for each plot unless an alternate plot layout is specified.
<pre lang="R" translate="no" dir="ltr">
<code>
<comment>### USAGE</comment>
dummy <- function(){
<comment># The test_plots are functions that make plots</comment>
	show(test_plot1, test_plot2, test_plot3)
	save(c(test_plot1(), test_plot2(), test_plot3()), <str>"cool_plots"</str>)
}
dummy()
</code>
</pre>
</div></div>

<div class="collapsible">
<div class="panel">
<h2> Exponential Regression </h2>

<pre lang="R" translate="no" dir="ltr">
<code>
<comment>## Data</comment>
y 	<- as.numeric(ydata)
x 	<- as.numeric(xdata)

<comment>## Max & Min values for limits</comment>
ymax <- max(y, na.rm=<bool>TRUE</bool>)
ymin <- min(y, na.rm=<bool>TRUE</bool>)

xmin <- min(x, na.rm=<bool>TRUE</bool>)
xmax <- max(x, na.rm=<bool>TRUE</bool>)

<comment>## Sequence between the minimum and maximum x value</comment>
newx 	<- seq(xmin, xmax, length.out=length(x))

<comment># Non-linear model (exponential)</comment>
plot(x,y, col=c(<str>"blueviolet"</str>), pch=<num>16</num>,
	xlim=c(xmin, xmax), ylim=c(ymin, ymax))

model.0 <- lm(log(y, base=exp(<num>1</num>))~x, data=data.frame(x,log(y, base=exp(<num>1</num>))))
start 	<- list(a=coef(model.0)[<num>1</num>], b=coef(model.0)[<num>2</num>])
model 	<- nls(y~a+b\*x, data=data.frame(x=x, y=log(y, base=exp(<num>1</num>))), start=start)

<comment>## Trendline</comment>
q 	<- coef(model)
curve(exp(q[<num>1</num>]+q[<num>2</num>]*x), col=<str>"Red"</str>, add=<bool>TRUE</bool>)

<comment>## Confidence Interval</comment>
confint <- predict(model.0, newdata=data.frame(x=newx), interval=<str>'confidence'</str>)

lines(newx, exp(confint[ ,<num>3</num>]), col=<str>"blue"</str>, lty=<str>"dashed"</str>)
lines(newx, exp(confint[ ,<num>2</num>]), col=<str>"blue"</str>, lty=<str>"dashed"</str>)

<comment>## Prediction Interval</comment>
predint <- predict(model.0, newdata=data.frame(x=newx), interval=<str>'prediction'</str>)

lines(newx, exp(predint[ ,<num>3</num>]), col=<str>"magenta"</str>, lty=<str>"dashed"</str>)
lines(newx, exp(predint[ ,<num>2</num>]), col=<str>"magenta"</str>, lty=<str>"dashed"</str>)
</code>
</pre>
</div></div>

<div class="collapsible">
<div class="panel">
<h2> The Pac-Man Residual plot </h2>

<pre lang="R">
<code>
<comment>## Data</comment>
y 	<- as.numeric(ydata)
x 	<- as.numeric(xdata)

<comment>## Max & Min values for limits</comment>
ymax    <- max(y, na.rm=<bool>TRUE</bool>)
ymin    <- min(y, na.rm=<bool>TRUE</bool>)

xmin    <- min(x, na.rm=<bool>TRUE</bool>)
xmax    <- max(x, na.rm=<bool>TRUE</bool>)

<comment>## Sequence between the minimum and maximum x value</comment>
newx 	<- seq(xmin, xmax, length.out=length(x))

<comment># Non-linear model (exponential)</comment>
plot(x,y, col=c(<str>"blueviolet"</str>), pch=<num>16</num>,
	xlim=c(xmin, xmax), ylim=c(ymin, ymax))

model.0 <- lm(log(y, base=exp(<num>1</num>))~x, data=data.frame(x,log(y, base=exp(<num>1</num>))))
start 	<- list(a=coef(model.0)[<num>1</num>], b=coef(model.0)[<num>2</num>])
model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(<num>1</num>))), start=start)

residual <- abs(resid(model))
t 	<- seq(<num>40</num>, <num>320</num>, len=length(residual))

rmax 	<- max(as.numeric(residual), na.rm=<bool>TRUE</bool>)
test    <- polar.plot(residual, t, rp.type=<str>"s"</str>,labels=<str>""</str>,
		radial.lim=c(<num>0</num>, <num>1</num>),show.grid=<bool>TRUE</bool>, show.grid.labels=<bool>FALSE</bool>,
		main=<str>"Pac-Man Residual Plot"</str>,
		show.radial.grid=<bool>FALSE</bool>, grid.col=<str>"black"</str>)

<comment>## Alternates Colors for contrast</comment>
color1 <- <str>"Yellow"</str>
color2 <- <str>"White"</str>
draw.circle(<num>0</num>, <num>0</num>, radius=<num>1.0</num>, col=color1)
draw.circle(<num>0</num>, <num>0</num>, radius=<num>0.8</num>, col=color2)
draw.circle(<num>0</num>, <num>0</num>, radius=<num>0.6</num>, col=color1)
draw.circle(<num>0</num>, <num>0</num>, radius=<num>0.4</num>, col=color2)
draw.circle(<num>0</num>, <num>0</num>, radius=<num>0.2</num>, col=color1)

<comment>## Plots output of residual against an arbitary angle</comment>
polar.plot(residual, t, rp.type=<str>"s"</str>,point.col=<str>"blue"</str>,
	point.symbols=<num>16</num>, add=<bool>TRUE</bool>)

<comment>## Labeling</comment>
text(c(<num>0.12</num>, <num>0.3</num>, <num>0.5</num>, <num>0.7</num>, <num>0.9</num>), <num>0</num>, 
	 c(<num>0.2</num>, <num>0.4</num>, <num>0.6</num>, <num>0.8</num>, <num>1.0</num>), cex=<num>1</num>)

<comment>## Defines region dedicated to labeling </comment>
polar.plot(c(<num>0</num>, <num>1</num>), c(min(t) - <num>10</num>, min(t) - <num>10</num>), lwd=<num>1</num>, 
			rp.type=<str>"p"</str>, line.col=<str>"black"</str>, add=<bool>TRUE</bool>)
polar.plot(c(<num>0</num>, <num>1</num>), c(max(t) + <num>10</num>, max(t) + <num>10</num>), lwd=<num>1</num>, 
			rp.type=<str>"p"</str>, line.col=<str>"black"</str>, add=<bool>TRUE</bool>)

</code>
</pre>
</div></div></div></div></div>
