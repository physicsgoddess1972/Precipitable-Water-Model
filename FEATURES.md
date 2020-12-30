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
Information regarding this feature is presented in more detail <a href="https://spencerriley.me/pacviz/book/">here</a>.
</div></div>
