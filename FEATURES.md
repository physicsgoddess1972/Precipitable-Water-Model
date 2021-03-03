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
<h2> Exponential Regression </h2>

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
legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
       exp(coef(exp_reg$model)[1]),
       coef(exp_reg$model)[2], exp_reg$R2)),
       "Confidence Interval"))
</code>
</pre>

</div></div>

<div class="collapsible">
<div class="panel">
<h2> The Pac-Man Residual plot </h2>
Information regarding this feature is presented in more detail <a href="https://spencerriley.me/pacviz/book/">here</a>.
</div></div>
