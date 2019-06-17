<a id="top"></a>
<h1>Precipitable Water Model</h1>

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

<div id="data">
<div class="collapsible">
<div class="collapsible-header">
		<h2>Introduction</h2>
</div>
<div class="panel">
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
</div></div>

<div id="data">
<div class="collapsible">
<div class="collapsible-header">
	<h2>Data Format</h2>
</div>
<div class="panel">
<div class="data-format">
The computational model that was developed to analyze the data collected uses a strict format. There are some cases
where the format can be more interpretive.

</div></div></div></div>

<div id="require">
<div class="collapsible">
<div class="collapsible-header">
	<h2>Requirements</h2>
</div>
<div class="panel">
To satisfy the requirements to execute the script. Run <code>install.sh</code>. 
It will install the system requirements and the R package 
requirements.

<pre lang="bash">
<code>
<inp>$</inp> bash install.sh
</code>
</pre>
</div></div></div>

<div id="overview">
<div class="collapsible">
<div class="collapsible-header">
	<h2>Overview of the Model</h2>
</div>
<div class="panel">
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
<inp>$</inp> Rscript model.r --help

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
<div class="panel">
<h3> 'Main' Set Contents </h3>
<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --set m
<inp>$</inp> Rscript model.r --set m --overcast
</code>
</pre>

This set of plots is divided into two subsets based on the condition labels.
In our case the conditions are clear skies and overcast.
To run this plot set use the terminal commands in this section. 
The overcast data can be seen via the
<code>--overcast</code> argument.

Both plot subsets include three plots 

<ol>
	<li> Air Temperature Time Series </li>
	<li> Ground Temperature Time Series </li>
	<li> Change in Temperature Time Series </li>
</ol>
</div></div>

<div class="collapsible">
<div class="panel">
<h3> 'Plots Galore' Set Contents </h3>
<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --set p
</code>
</pre>

<ol>
	<li> Individual Location PW and Temperature </li>
	<li> Locational Average PW and Temperature </li>
	<li> Total Mean PW and Temperature </li>
	<li> Residual for Total Mean PW and Temperature</li>
	<li> Pac-Man Residual for Total Mean PW and Temperature </li>
</ol>
</div></div>

<div class="collapsible">
<div class="panel">
<h3> 'Other' Set Contents </h3>

<pre lang="bash">
<code>
<inp>$</inp> Rscript model.r --set o
</code>
</pre>

<ol>
	<li> Overcast Condition Percentage (Bar) </li>
	<li> Overcast Condition Percentage (Pie) </li>
</ol>
</div></div>

<div id="step">
<div class="collapsible">
<div class="panel">
<h3> Step-by-Step usage (Linux)</h3>
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
<tr style="border: 0px;">
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



<div id="contrib">
<div class="collapsible">
<div class="collapsible-header">
<h2>Contributing to the Research</h2>
</div>
<div class="panel">
If you would like to contribute to this project, visit our <a href="./contrib.html">contribution page</a>.
</div></div></div>

<div id="next">
<div class="collapsible">
<div class="collapsible-header">
<h2>Next Steps</h2>
</div>
<div class="panel">
The future development of this project with regards to the data collection include 
</div></div></div>

<div id="resource">
<div class="collapsible">
<div class="collapsible-header">
    <h2>Resources</h2>
</div>
<div class="panel">
    <ul>
        <li><a href="" target="_blank">Wyoming Sounding Data</a></li>
    </ul>
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

