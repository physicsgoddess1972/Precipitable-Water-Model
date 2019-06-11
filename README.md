# Precipitable Water Model

[comment]: # (Precipitable-Water-Model)
[comment]: # (|)
[comment]: # (|--- data/)
[comment]: # (|   |--- instruments.txt)
[comment]: # (|   |--- master_data.csv)
[comment]: # (|   `--- ml_data.csv)
[comment]: # (|)
[comment]: # (|--- install.sh)
[comment]: # (|)
[comment]: # (|--- README.md)
[comment]: # (|)
[comment]: # (`--- src/)
[comment]: # (	|--- archive/)
[comment]: # (	|   |--- main.py)
[comment]: # (	|   |--- mrop.py)
[comment]: # (	|   `--- plots_galore.py)
[comment]: # (	`--- model.r)

<div id="intro">
<div class="collapsible">
<div class="collapsible-header">
    <h2 id="intro">Introduction</h2><a id="top" class="material-icons" href="#">arrow_upward</a>
</div>
<div class="collapsible-body">
<h3>Goal</h3>
The goal of this project is to determine the correlation between
zenith sky temperature and the precipitable water. This experiment 
is based off of a similar study conducted by Mims (?...?). 
<h3>Intstrumentation</h3>
This experiment used three infrared sensors:
<ol>
    <li>AMES</li>
    <li>FLi3R</li>
    <li>1610 TE</li>
</ol>
The purpose of these sensors is to measure the thermal energy of a 
given area in the atmosphere. The area is determined by the Distance to
Spot ratio.
</div></div></div>

<div id="data">
<div class="collapsible">
<div class="collapsible-header">
    <h2>Data Format</h2><a id="top" class="material-icons" href="#">arrow_upward</a>
</div>
<div class="collapsible-body">

</div></div></div>

<div id="require">
<div class="collapsible">
<div class="collapsible-header">
    <h2>Requirements</h2><a id="top" class="material-icons" href="#">arrow_upward</a>
</div>
<div class="collapsible-body">
To satisfy the requirements to execute the script. Run <code>install.sh</code>. 
It will install the system requirements and the R package 
requirements.

<pre lang="bash">
<code>
$ bash install.sh
</code>
</pre>
</div></div></div>

<div id="overview">
<div class="collapsible">
<div class="collapsible-header">
    <h2>Overview of the Model</h2><a id="top" class="material-icons" href="#">arrow_upward</a>
</div>
<div class="collapsible-body">
<b>Please read this section before using the script</b>
<br />
The computational model is enclosed in the script <code>model.r</code>. 
There are some minor difficulties that you may encounter using this script, 
many of these difficulties relate to the plotting window. 
The first is important, resizing the plot display will cause the window to blank.
The quick fix is to just re-run the script. There is currently no permanent fix for this bug.
The second issue is seemingly random. Upon running the script if one or more of the plot displays is blank 
re-run the script until there are no blank windows. 
It should be noted that these issues only impacts the display of the plots and will not affect plots that are saved.

<pre lang="bash">
<code>
$ Rscript model.r --help

usage: model.r [-h] [--save] [--set SET] [--poster] [--dev] [-d] [-o] [-w] [-i]

optional arguments:
  -h, --help      show this help message and exit
  --save          Saves plots
  --set SET       Select plot sets: [m]ain/[p]lots_galore/[o]ther
  --poster        Produces poster plots
  --dev           Development plots
  -d, --data      Produces two columned dataset including mean temp and PW
  -o, --overcast  Shows time series data for days with overcast condition
				  (Used with --set m)
  -w, --warning	  Shows warnings associated with the script
  -i, --instrument	Prints out sensor data stored in instruments.txt
</code>
</pre>

<div class="collapsible">
<div class="collapsible-body">
<h3> 'Main' Set Contents </h3>
<pre lang="bash">
<code>
$ Rscript model.r --set m
$ Rscript model.r --set m --overcast
</code>
</pre>

This set of plots is divided into two subsets based on the condition labels.
In our case the conditions are clear skies and overcast.
To run this plot set use the terminal commands in this section. 
The overcast data can be seen via the
```--overcast``` argument.

Both plot subsets include three plots 

<ol>
    <li> Air Temperature Time Series </li>
    <li> Ground Temperature Time Series </li>
    <li> Change in Temperature Time Series </li>
</ol>
</div>
<div class="collapsible-body">
<h3> 'Plots Galore' Set Contents </h3>
<pre lang="bash">
<code>
 $ Rscript model.r --set p
</code>
</pre>

<ol>
    <li> Individual Location PW and Temperature </li>
    <li> Locational Average PW and Temperature </li>
    <li> Total Mean PW and Temperature </li>
    <li> Residual for Total Mean PW and Temperature</li>
    <li> Pac-Man Residual for Total Mean PW and Temperature </li>
</ol>
</div>
<div class="collapsible-body">
<h3> 'Other' Set Contents </h3>

<pre lang="bash">
<code>
$ Rscript model.r --set o
</code>
</pre>

<ol>
    <li> Overcast Condition Percentage (Bar) </li>
    <li> Overcast Condition Percentage (Pie) </li>
</ol>
<div class="step">
<h3> Step-by-Step usage </h3>
<table class="usage">
<tbody>
<tr style="border: 0px;">
    <td>
        <span class="numbered">1</span>
    </td>
    <td>
        Fork, Clone, or Download the repository.
    </td>
</tr>
<tr>
    <td>
        <span class="numbered">2</span>
    </td>
    <td>
        In your terminal inside of project directory run
        <code>bash install.sh</code>.
    </td>
</tr>
<tr>
    <td>
        <span class="numbered">3</span>
    </td>
    <td>
        Update <code>instruments.txt</code>with the appropriate
        sensor information. 
    </td>
</tr>
<tr>
    <td>
        <span class="numbered">4</span>
    </td>
    <td>
        Update <code>master_data.csv</code> with your collected
        data following the format earlier defined in 
        <a href="#data">Data Format</a>. </li>
    </td>
</tr>
<tr>
    <td>
        <span class="numbered">5</span>
    </td>
    <td>
        To view the plots see the above subsections for 
        the appropriate command line arguments. </li>
    </td>
</tr>
</tbody>
</table>
</div></div></div></div></div></div>


<div id="r-feat">
<div class="collapsible">
<div class="collapsible-header">
    <h2>R Features</h2><a id="top" class="material-icons" href="#">arrow_upward</a>
</div>
<div class="collapsible-body">
The following sections define and show interesting 
features in the R source code
that exist as a part of the model. To use the following 
code snippets it is important to run the bash script
defined in <a href="#require">Requirements</a>. 

<div class="collapsible">
<div class="collapsible-body">
<h3> Show and Save Functions </h3>
This collection of functions uses the X11 framework 
to produce pop-up windows of the visual outputs of
the R script. The <code>show()</code> function produces the 
plotting window, and the <code>continue_input()</code> function
works to keep the window open until the Enter key is
inputted into the terminal. Without the <code>continue_input()</code>
function, the plotting window would automatically open and close, ending the 
script. 
<pre lang="R">
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
<pre lang="R">
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
</div>

<div class="collapsible-body">
<h3> Exponential Regression </h3>

<pre lang="R">
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
model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(<num>1</num>))), start=start)

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

<h3> The Pac-Man Residual plot </h3>

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

<div id="contrib">
<div class="collapsible">
<div class="collapsible-header">
<h2>Contributing to the Research</h2><a id="top" class="material-icons" href="#">arrow_upward</a>
</div>
<div class="collapsible-body">
</div></div></div>

<div id="next">
<div class="collapsible">
<div class="collapsible-header">
<h2>Next Steps</h2><a id="top" class="material-icons" href="#">arrow_upward</a>
</div>
<div class="collapsible-body">
The future development of this project with regards to the data collection include 
</div></div></div>

<div id="footer">
    <hr style="border-color: rgba(46,156,202,0.96); width: 100%; margin-left: -20rem;">
    <div style="margin-left: -1rem; width: 100%; margin-right: -20rem;">
        <h2>The Maintainers</h2>
    </div>
    <table class="maintain">
        <tbody>
            <tr style="border: 0px;">
                <td>
                    <i class="material-icons">face</i> 
                </td>
                <td>
                    Spencer Riley
                </td>
                <td>
                    <i class="material-icons">face</i>
                </td>
                <td>
                    Vicki Kelsey
                </td>
            </tr>
            <tr>
                <td>
                    <i class="material-icons">public</i>
                </td>
                <td>
                    <a target="_blank" href="http://pharaohcola13.github.io">pharaohcola13.github.io</a>
                </td>
                <td>
                    <i class="material-icons">public</i>
                </td>
                <td>
                    <a target="_blank" href="http://physicsgoddess1972.github.io">physicsgoddess1972.github.io</a>
                </td>
            </tr>
            <tr>
                <td>
                    <i class="material-icons">alternate_email</i>
                </td>
                <td>
                    spencer.riley@student.nmt.edu
                </td>
                <td>
                    <i class="material-icons">alternate_email</i>
                </td>
                <td>
                    vicki.kelsey@student.nmt.edu
                </td>
            </tr>
        </tbody>
    </table>
</div>
