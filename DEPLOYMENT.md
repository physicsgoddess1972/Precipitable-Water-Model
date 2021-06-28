<a id="top"></a>

<div id="deploy-docs">
    <div class="collapsible">
        <div class="collapsible-header">
            <h2>Deployment</h2>
        </div>
        <div class="panel">
            With the application of Docker containers in the project, we have evolved our deployment system to be much more easily understandable for users. We utilize the GitHub user interface for automating a significant amount of the data processing and analysis. 
<br><br>
As a result we now have two methods of deploying PMAT, the first and recommended method is through GitHub's interface. This requires zero installation of external programs and can be deployed virtually anywhere with ease. The second method of deployment is through local installation. This requires the installation of Python 3 and R in addition to several other dependencies and will only operate on Ubuntu or Debian operating systems. 
        </div>
    </div>
</div>
<div id="github">
    <div class="collapsible">
        <a href="#github">
            <div class="collapsible-header">
                <h2><i class="devicon-github-original colored"></i>Github Deployment</h2>
            </div>
        </a>
        <div class="panel">
            This version of the Deployment module is, for the most part, automated and recommended. Follow the steps in this section to successfully deploy PMAT through GitHub. 
<br><br>
<b>We should make note that the only prerequisite for this deployment system is a GitHub account. </b>
<table class="gh-deploy">
<tbody>
<tr style="border: 0px;">
	<td><span class="numbered">1</span></td>
	<td>Create an <a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/issues">Issue</a> using the PMAT Deployment template. A branch with the city and state name will be created, all branches of the repo can be viewed <a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/branches">here.</a> </td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
	<td>Once the Issue has been closed, <a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/fork">fork the repository</a></td>
</tr>
<tr>
	<td><span class="numbered">3</span></td>
	<td>Navigate to the branch that corresponds to your location and edit the data files appropriately. Guidelines for data files can be viewed on the <a href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/">main documentation page</a>.</td>
</tr>
<tr>
	<td><span class="numbered">4</span></td>
	<td>Submit a pull request. Be sure when opening a pull request that the <code>base</code> and <code>head</code> repositories have branches that correspond to the branch created in Step 1. </td>
</tr>
</tbody>
</table>
        </div>
    </div>
</div>
<div id="linux">
    <div class="collapsible">
        <div class="collapsible-header">
            <h2><i class="devicon-ubuntu-plain colored"></i>Local Deployment</h2>
        </div>
        <div class="panel">
<table class="local-deploy">
<tbody>
<tr style="border: 0px;">
	<td><span class="numbered">1</span></td>
	<td>Clone the repository</td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
	<td>Navigate to the <code>util/</code> directory in the Project folder</td>
</tr>
<tr>
	<td><span class="numbered">3</span></td>
	<td>Run the bash command <code>bash setup.sh -a</code></td>
</tr>
<tr>
	<td><span class="numbered">4</span></td>
	<td>Create and fill out the configuration files: <code>instruments.yml</code> and <code>import.conf</code>.</td>
</tr>
<tr>
    <td><span class="numbered">5</span></td>
    <td>Input all data collected into <code>data/cool_data.csv</code></td>
</tr>
<tr>
    <td><span class="numbered">6</span></td>
    <td>Navigate to the <code>src/util/</code> directory and run the python script <code>instruments.py</code></td>
</tr>
<tr>
    <td><span class="numbered">7</span></td>
    <td>Navigate to the <code>src/</code> directory and run the appropriate commands for the designated plots or data as described in the following section.</td>
</tr>
</tbody>
</table>
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
