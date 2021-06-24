<a id="top"></a>

<div id="deploy-docs">
    <div class="collapsible">
        <div class="collapsible-header">
            <h2>Deployment</h2>
        </div>
        <div class="panel">
            This section is currently under development
        </div>
    </div>
    <div class="collapsible">
        <div class="collapsible-header">
            <h2><i class="devicon-github-original colored"></i>Github Deployment</h2>
        </div>
        <div class="panel">
            This section is currently under development
        </div>
    </div>
    <div class="collapsible">
        <div class="collapsible-header">
            <h2><i class="devicon-ubuntu-plain colored"></i>Local Deployment</h2>
        </div>
        <div class="panel">
<b>Please read this section before using the script</b>
<br />
The computational model is enclosed in the script <code>model.r</code>.
Some of the plot sets are divided into two subcategories: clear sky and overcast.
This division is used to isolate data where clouds may have interfered with the temperature
measurement. To access the overcast subcategory use the <code>--overcast</code> or <code>-o</code>
argument.
<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --help

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
<inp>$</inp> Rscript model.r --set t
<inp>$</inp> Rscript model.r --set t --overcast
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
</div></div>

<div class="collapsible_1">
<div class="panel">
<h3> 'Analytics' Set Contents </h3>
<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --set a
<inp>$</inp> Rscript model.r --set a --overcast
</code>
</pre>

<ol>
	<li> Individual Location PW and Temperature </li>
	<li> Locational Average PW and Temperature </li>
	<li> Total Mean PW and Temperature </li>
	<li> Residual for Total Mean PW and Temperature</li>
</ol>
</div></div>

<div class="collapsible_1">
<div class="panel">
<h3> 'Charts' Set Contents </h3>

<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --set c
</code>
</pre>

<ol>
	<li> Overcast Condition Percentage Bar Chart for each sensor</li>
</ol>
</div></div>

<div class="collapsible_1">
<div class="panel">
<h3> 'Individual Sensors' Set Contents </h3>

<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --set i
<inp>$</inp> Rscript model.r --set i --overcast
</code>
</pre>

<ol>
	<li> Sky and Ground Temperature Time Series for each sensor</li>
</ol>
</div></div>

<div class="collapsible_1">
<div class="panel">
<h3> 'Pac-Man' Set Contents </h3>

<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --pacman
<inp>$</inp> Rscript model.r --pacman --overcast
</code>
</pre>

<ol>
	<li>Total Mean PW and Temperature</li>
	<li>Pac-Man Residual Plot</li>
</ol>
</div></div>

</div></div></div></div>
        </div>
    </div>
</div>
