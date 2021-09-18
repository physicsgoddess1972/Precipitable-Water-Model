<a id="top"></a>

<div id="r-feat">
<div class="collapsible">
<div class="collapsible-header">
	<h2>Features</h2>
</div>
<div class="panel">
The following sections define and show interesting
features in the R source code
that exist as a part of the model. To use the following
code snippets it is important to run the bash script
defined in Methodology section of the <a href="./index.html">Documentation page</a>.
<br><br>
In a more general sense, the features included in the computational model are related to
methods of generalized data input. This is significant because it allows flexibility in the
number of sensors and precipitable water measurements that can be used. In other words,
two distinct groups, one using three infrared sensors and the other using one hundred sensors, can use
the same model with zero changes to the source code.
</div></div>

<div class="collapsible">
    <div class="collapsible-header">
        <h2>Contents of PMAT</h2>
    </div>
	<div class="panel">
        The computational model is enclosed in the scripts: <code>pmat_processing</code>, <code>pmat_analysis.r</code>, <code>pmat_plots.r</code>, <code>pmat_run.r</code>.
        Some of the plot sets are divided into two subcategories: clear sky and overcast.
        This division is used to isolate data where clouds may have interfered with the temperature
        measurement. To access the overcast subcategory use the <code>--overcast</code> or <code>-o</code>
        argument.
        <pre lang="bash">
            <code>
<inp>$</inp> Rscript pmat_run.r --help

usage: model.r [-h] [--set SET] [--poster] [--dev] [-d] [-o] [-1st] [-i] [-ml]
            [--pacman]

optional arguments:
-h, --help          Show this help message and exit
--set SET           Select plot sets:
                        [t]ime series
                        [a]nalytics
                        [c]harts
                        [i]ndividual sensors
--poster            Produces poster plots
--dev               Development plots
-d, --data          Produces two columned dataset including mean temp and PW
-o, --overcast      Shows time series data for days with overcast condition
                    (Used with --set [t/a/i])
-1st, --first_time  Notes for first time users.
-i, --instrument    Prints out sensor data stored in instruments.yml
-ml                 Outs a datafile to use with the machine learning algorithm.
--pacman            Produces Pacman plots.
            </code>
        </pre>
        <div class="collapsible_1">
            <div class="panel">
                <h3> 'Time Series' Set Contents </h3>
                <pre lang="bash">
                    <code>
<inp>$</inp> Rscript pmat_run.r --set t
<inp>$</inp> Rscript pmat_run.r --set t --overcast
                    </code>
                </pre>
                <ol>
                    <li> Air Temperature Time Series </li>
                    <li> Ground Temperature Time Series </li>
                    <li> Change in Temperature Time Series </li>
                    <li> Precipitable Water Time Series </li>
                    <li> Sky Temperature - Precipitable Water Time Series </li>
                    <li> Temporal Mean Precipitable Water Time Series </li>
                    <li> Locational Mean Precipitable Water Time Series </li>
                    <li> Mean Precipitable Water Time Series </li>
                    <li> Precipitable Water  - RH Time Series</li>
                    <li> Sky Temperature - RH Time Series </li>
                </ol>
            </div>
        </div>
        <div class="collapsible_1">
            <div class="panel">
                <h3> 'Analytics' Set Contents </h3>
                <pre lang="bash">
                    <code>
<inp>$</inp> Rscript pmat_run.r --set a
<inp>$</inp> Rscript pmat_run.r --set a --overcast
                    </code>
                </pre>
                <ol>
                    <li> Individual Location PW and Temperature </li>
                    <li> Locational Average PW and Temperature </li>
                    <li> Total Mean PW and Temperature </li>
                    <li> Residual for Total Mean PW and Temperature</li>
                </ol>
            </div>
        </div>
        <div class="collapsible_1">
            <div class="panel">
                <h3> 'Charts' Set Contents </h3>
                <pre lang="bash">
                    <code>
<inp>$</inp> Rscript pmat_run.r --set c
                    </code>
                </pre>
                <ol>
                    <li> Overcast Condition Percentage Bar Chart for each sensor</li>
                </ol>
            </div>
        </div>
        <div class="collapsible_1">
            <div class="panel">
                <h3> 'Individual Sensors' Set Contents </h3>
                <pre lang="bash">
                    <code>
<inp>$</inp> Rscript pmat_run.r --set i
<inp>$</inp> Rscript pmat_run.r --set i --overcast
                    </code>
                </pre>
                <ol>
                    <li> Sky and Ground Temperature Time Series for each sensor</li>
                </ol>
            </div>
        </div>
        <div class="collapsible_1">
            <div class="panel">
                <h3> 'Pac-Man' Set Contents </h3>
                <pre lang="bash">
                    <code>
<inp>$</inp> Rscript pmat_run.r --pacman
<inp>$</inp> Rscript pmat_run.r --pacman --overcast
                    </code>
                </pre>
                <ol>
                    <li>Total Mean PW and Temperature</li>
                    <li>Pac-Man Residual Plot</li>
                </ol>
            </div>
        </div>
    </div>
</div>

<div class="collapsible">
    <div class="collapsible-header">
        <h2> Exponential Regression </h2>
    </div>
    <div class="panel">
        <pre lang="R" translate="no" dir="ltr">
            <code>
exp_regression 	<- function(x,y){
    <comment># Finds and removes NaNed values from the dataset</comment>
    nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
    x <- x[-(nans)]; y <- y[-(nans)]
    <comment># creates a uniform sequence of numbers that fit within the limits of x</comment>
    xmin 	<- min(x, na.rm=TRUE)
    xmax 	<- max(x, na.rm=TRUE)
    newx 	<- seq(xmin, xmax, length.out=length(x))
    <comment># Non-linear model (exponential)</comment>
    model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x=x, y=y))
    <comment>## Initial values are in fact the converged values</comment>
    start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
    model 	<- nls(log(y, base=exp(1))~a+x*b, data=data.frame(x=x, y=y), start=start)
    <comment># Intervals (confidence/prediction)</comment>
    confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
    predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')
    <comment># Coefficient of determination</comment>
    r2		<- summary(model.0)$r.squared
    <comment># Function outputs</comment>
    output 	<- list("x"=x, "y"=y,
                    "newx"=newx,
                "model.0"=model.0,
                "model"=model,
                "confint"=confint,
                "predint"=predint,
                "R2"=r2)
    return (output)
}
exp_reg <- exp_regression(xdata, ydata)
<comment># Non-linear model (exponential)</comment>
plot(exp_reg$x, exp_reg$y, pch=1)
<comment># Best Fit</comment>
curve(exp(coef(exp_reg$model)[1]+(coef(exp_reg$model)[2]*x)), col="black", add=TRUE)
<comment># Confidence Interval </comment>
lines(exp_reg$newx,
    exp(exp_reg$confint[ ,3]),
    col="black",
    lty="dashed")
lines(exp_reg$newx,
    exp(exp_reg$confint[ ,2]),
    col="black",
    lty="dashed")
<comment># Predicition Interval</comment>
polygon(c(exp_reg$newx, rev(exp_reg$newx)),
        c(exp(exp_reg$predint[ ,3]),
        rev(exp(exp_reg$predint[ ,2]))),
        col=rgb(0.25, 0.25, 0.25,0.25),
        border = NA)
<comment># Legend</comment>
legend("topleft",col=c("black", "black"), lty=c(1,2),
legend=c(parse(text=sprintf("%.2f*e^{"%.3"f*x}*\t\t(R^2 == %.3f)",
    exp(coef(exp_reg$model)[1]),
    coef(exp_reg$model)[2], exp_reg$R2)),
    "Confidence Interval"))
            </code>
        </pre>
    </div>
</div>

<div class="collapsible">
    <div class="collapsible-header">
        <h2> The Pac-Man Residual plot </h2>
    </div>
    <div class="panel">
        Information regarding this feature is presented in more detail <a href="https://pacviz.sriley.dev">here</a>.
    </div>
</div>
