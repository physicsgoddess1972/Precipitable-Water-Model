<a id="top"></a>
<div id="data">
<div class="collapsible">
<div class="collapsible-header">
		<h2>Introduction</h2>
</div>
<div class="panel">
    <h3>What is Precipitable Water?</h3>
    Precipitable water is the amount of condensed water vapor to be found in a vertical column of air, with a base of 1 meter-squared, that
    extends from the surface of the Earth to the top of the atmosphere.
    <br><br>
    <img src="https://i.ibb.co/3FF6wTC/tpw-image5.png" width=80% height=50% style="display: block; margin-left: auto; margin-right: auto"/>    
	<br>
    <i><b>Figure 1:</b> Schematic illustrating the concept of precipitable water. The left column contains air and water vapour,
        the right column contains dry air and condensed water vapour on the bottom of the column <a href="#references">[1]</a>.
    </i>
    <br><br>
    Precipitable water is important because:
    <ul>
        <li> Energy is transferred from the surface to the atmosphere via water vapor, and is released as latent heat. Precipitable water helps
     us determine the amount of energy in the atmosphere. </li>
        <li> Weather forecasting models can use precipitable water data to determine the likelihood of storms, hail, and other major meteorological
        events. </li>
        <li> The relationship between air temperature and the amount of water vapor is linear. Therefore, precipitable water measurements can be
        used to determine temperature increases at higher altitudes.
    </ul>
	<h3>Goal</h3>
	The goal of this project is to determine the correlation between
	zenith sky temperature and precipitable water. This experiment
	is based off of a similar study conducted by Mims et al <a href="#references">[2]</a>.
	We endeavor to develop a methodology and data
	source that is more rigorous, more accessible, and more easily repeatable across a variety of climate zones.
	<h3>Intstrumentation</h3>
    <img src="https://i.ibb.co/s2qTzgy/thermometers.jpg" width=80% height=50% style="display: block; margin-left: auto; margin-right: auto"></img>
    <br>
	This experiment used three infrared sensors <i>(from left to right)</i>:
	<ol>
		<li>1610 TE</li>
		<li>FLIR i3</li>
		<li>AMES</li>
	</ol>
	The purpose of these sensors is to measure the thermal energy of a
	given area in the atmosphere. The area is determined by the Distance to
	Spot ratio.
	<br /><br />
    When using the model for your analysis, take the time to fully complete the
    <code>instruments.conf</code>
	file with the appropriate information. This will assure that the data
	properly corresponds to the labels of the sensors. If there is an entry
	that you are unable to fill, please use NA as a filler. More information
	regarding the different columns of the <code>instruments.conf</code> will
	be discussed in the Data Format section of this documentation page.
</div></div>
<div id="methods">
<div class="collapsible">
<div class="collapsible-header">
	<h2 id="methodology">Methodology</h2>
</div>
<div class="panel">
<div class="data-format">
    <h3>Setting Guidelines</h3>
<table class="usage">
<tbody>
<tr style="border: 0px;">
	<td><span class="numbered">1</span></td>
	<td>Determine the scope of project and reporting frequency</td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
	<td>Find the closest 2 or 3 precipitable water measuring sites for your area and familiarize yourself with how to access and read the data.</td>
</tr>
<tr>
	<td><span class="numbered">3</span></td>
	<td>Determine daily measurement site and time, to ensure consistent measurements happen.</td>
</tr>
<tr style="border: 0px;">
	<td><span class="numbered">4</span></td>
	<td>Decide on different infrared thermometers to take measurements with</td>
</tr>
</tbody>
</table>
<h3>Experimental Procedure</h3>
<table class="usage">
<tbody>
<tr style="border: 0px;">
	<td><span class="numbered">1</span></td>
	<td>
		At the same location and time take both ground and zenith sky temperature readings with the thermometers <i>(Ensure that the sun is not directly above when taking your readings)</i>
	</td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
	<td>
    Record the ground and zenith sky temperature for each thermometer
    <br />
    <i>(If the zenith sky reading is obstructed by cloud cover, record the condition as overcast. Otherwise record the condition as clear sky)</i>
    <br />
    <i>(Do not leave blanks in your dataset. Any value that is not available needs to be marked as NaN)</i>
	</td>
</tr>
<tr style="border: 0px;">
	<td><span class="numbered">3</span></td>
	<td>Retrieve precipitable water readings and add them to your dataset</td>
</tr>
</tbody>
</table>
<h3>Data Analysis</h3>
<table class="usage">
<tbody>
<tr style="border: 0px;">
	<td><span class="numbered">1</span></td>
	<td>In the Linux terminal run the command <code>bash setup.sh -a</code></td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
	<td>Update <code>instruments.conf</code>with the appropriate sensor information.</td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
	<td>Download your dataset as a Comma-Seperated-Value file <i>(.csv)</i>, with the filename <code>cool_data.csv</code>. <i>(Be sure to follow the guidelines laid out in <a href="#data">Data Format</a>.)</i></td>
</tr>
<tr>
	<td><span class="numbered">3</span></td>
	<td>Transfer the datafile to <code>/data/</code> located inside of the Precipitable Water Model directory.</td>
</tr>
<tr style="border: 0px;">
	<td><span class="numbered">4</span></td>
	<td>From <code>/src/</code> run the command <code>bash run.sh -a</code> in the terminal. <i>(Go to <a>Model Overview</a> for more information</i>)</td>
</tr>
</tbody>
</table>
</div></div></div></div>

<div id="data">
<div class="collapsible">
<div class="collapsible-header">
	<h2>Data Format</h2>
</div>
<div class="panel">
<div class="data-format">
Using pattern identification, the data format is flexible with few strict requirements.
Here are some examples of valid datasets:
<br />
<a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/master/data/example/example1.csv" target="_blank">Dataset 1</a>
<br />
<a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/master/data/example/example2.csv" target="_blank">Dataset 2</a>
<br />
<a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/master/data/example/example1.csv" target="_blank">Dataset 3</a>
<br /><br />
It should be noted that the columns do not have to be in any set order, with one small caveat, the model pulls the data from columns with headers containing specific words or phrases. The caveat is with regards to Ground and Sky temperature readings. The temperature measurements must go in consecutive order by sensor as determined by <code>instruments.conf</code>.
<br /><br />
For example, if the order of the sensors in <code>instruments.conf</code> is 1610 TE, FLIR i3, and then AMES 1. Then the order of the ground and sky temperature measurements in the dataset should be: 1610 TE, FLIR i3, and then AMES 1. <i>(As seen in Dataset 2)</i>
<h3>Column Headers for Dataset</h3>
<table>
	<tbody>
		<tr>
			<td><b>Data Type</b></td>
			<td><b>Header</b></td>
			<td><b>Format</b></td>
		</tr>
		<tr>
			<td><b>Date</b></td>
			<td>Date</td>
			<td><code>MM/DD/YYYY</code></td>
		</tr>
		<tr>
			<td><b>Time</b></td>
			<td>Time</td>
			<td><code>HH:MM</code></td>
		</tr>
		<tr>
			<td><b>Condition</b></td>
			<td>Condition</td>
			<td>clear sky / overcast</td>
		</tr>
		<tr>
			<td><b>Sky Temperature</b></td>
			<td>Sensor Name<sup>\*</sup> (Sky)</td>
			<td>Number</td>
		</tr>
		<tr>
			<td><b>Ground Temperature</b></td>
			<td>Sensor Name<sup>\*</sup> (Ground)</td>
			<td>Number</td>
		</tr>
	</tbody>
</table>
<sup>\*</sup>This should be consistent with what the sensor is labeled as in <code>instruments.conf</code>
<h3>Format of <code>instruments.conf</code></h3>
The purpose of this file is to get all of the information on the sensors organized in one place. This file has a total
of six columns. Each row corresponds to a infrared thermometer used to collect data.
<li>The first column is used for designating the Sensor. If there is more than one sensor of the same type than use a underbar and the number. For example, if there are two sensors called AMES, then the you would type in `AMES_1` and `AMES_2`. </li>

<li>The Second column is the error of the sensor in units of Celsius. If an error cannot be recorded use `NA`. </li>

<li>The third column is the color code, this is a hexidecimal color that will be used in sensor specific plots. An example is of an input is `FF00FF`. </li>
<li>The fourth column is the Distance ratio of the sensor, this is often recorded in the manual or on the packaging. In the file record the ratio in the format `a:b` with `a` and `b` corresponding to the numbers for the sensor.</li>
<li>The fifth column is a `True` or `False` input that will decide whether or not to include the sensor in the poster plots</li>
<li>The last column is for the temperature range in Celsius.</li>
<h3>Important Note for Missing Data</h3>
If you have a sensor that did not take measurements for a period of time, for whatever reason, use `-Inf` as the measurement for that day. For example, if the battery of AMES 1 died for two days the ground and sky measurement for those days would be `-Inf`.
</div></div></div></div>

<div id="overview">
<div class="collapsible">
<div class="collapsible-header">
	<h2>Overview of the Model</h2>
</div>
<div class="panel">
<b>Please read this section before using the script</b>
<br />
The computational model is enclosed in the script <code>model.r</code>.
Some of the plot sets are divided into two subcategories: clear sky and overcast.
This division is used to isolate data where clouds may have interfered with the temperature
measurement. To access the overcast subcategory use the <code>--overcast</code> or <code>-o</code>
argument.
<br /><br />

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
  -i, --instrument    Prints out sensor data stored in instruments.conf
  -ml                 Outs a datafile to use with the neural network.
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
The future development of this project with regards to the data collection include a machine learning approach to determining weather condition <i>(Clear sky/Overcast)</i>. We are also in the process of developing a method of automating the temperature measurement process using an Arduino-Raspberry Pi network.
</div></div></div>

<div id="resource">
<div class="collapsible">
<div class="collapsible-header">
    <h2>Resources</h2>
</div>
<div class="panel">
    <ul>
        <li><a href="http://weather.uwyo.edu/upperair/sounding.html" target="_blank">Wyoming Sounding Data</a></li>
    </ul>
</div></div></div>

<div id="references">
<div class="collapsible">
<div class="collapsible-header">
    <h2>References</h2>
</div>
<div class="panel">
[1] Mária, P. (n.d.). Product Tutorial on TPW Content Products.
<br /><br />
[2] Forrest M. Mims, Lin Hardtung Chambers, and David R. Brooks.  Measuring total column water vapor by pointing an infrared thermometer at the sky.<i>Bulletin of the American Meteorological Society</i>, 92(10):1311-1320, 2011.
</div></div></div></div>
