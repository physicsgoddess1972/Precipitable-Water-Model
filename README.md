<a id="top"></a>
<div>
    <div class="collapsible">
        <div class="collapsible-header">
                <h2>Introduction</h2>
        </div>
        <div class="panel">
            <p style="text-align: center">
            <a href="https://zenodo.org/badge/latestdoi/178975498"><img src="https://zenodo.org/badge/178975498.svg" alt="DOI"></a>
            <a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/releases/tag/v2.0"><img alt="GitHub tag (latest by date)" src="https://img.shields.io/github/v/tag/physicsgoddess1972/Precipitable-Water-Model?label=PMAT"></a>
		<p style="text-align: center"><a href="https://github.com/ellerbrock/open-source-badges/"><img src="https://badges.frapsoft.com/os/v1/open-source.svg?v=103" alt="Open Source Love svg1"></a></p>
            <h3>What is Precipitable Water?</h3>
            Precipitable water is the amount of condensed water vapor to be found in a vertical column of air, with a base of 1 meter-squared, that
            extends from the surface of the Earth to the top of the atmosphere.
            <br><br>
            <img src="https://i.ibb.co/3FF6wTC/tpw-image5.png" width="100%" style="display: block; margin-left: auto; margin-right: auto"/>    
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
            <img src="https://i.ibb.co/s2qTzgy/thermometers.jpg" width="100%" style="display: block; margin-left: auto; margin-right: auto">
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
            <code>_pmat.yml</code>
            file with the appropriate information. This will assure that the data
            properly corresponds to the labels of the sensors. If there is an entry
            that you are unable to fill, please use NA as a filler. More information
            regarding the different columns of the <code>_pmat.yml</code> will
		be discussed in the <a href="https://docs.pmat.app/data-formatting" target="_blank">Working with data and PMAT</a> section of the Official PMAT Documentation.
        </div>
    </div>
</div>
<div id="pmat">
    <div class="collapsible">
        <div class="collapsible-header">
            <h2 id="what-is-pmat">What is PMAT?</h2>
        </div>
        <div class="panel">
            The Precipitable-water Model Analysis Tool (PMAT) is a computational utility that is used to analyze the data collected from this project to understand the relationship between the zenith sky temperature and precipitable water in the atmosphere. For more information regarding PMAT, visit the <a href="https://docs.pmat.app" target="_blank">Official Documentation Page</a>.
        </div>
    </div>
</div>

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
	<td>Identify the closest MesoWest and wyoming sites to your area.</td>
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
    <i>(Do not leave blanks in your dataset. Any value that is not available needs to be marked as -Inf or NaN)</i>
	</td>
</tr>
<tr>
	<td><span class="numbered">3</span></td>
	<td>Retrieve precipitable water readings and add them to your dataset</td>
</tr>
</tbody>
</table>
<h3>Data Analysis</h3>
To complete the data analysis look at the <a href="https://docs.pmat.app/getting-started">procedures for PMAT deployment</a> and follow the steps that best fit your needs.
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
	<li><a href="http://weather.uwyo.edu/upperair/sounding.html" target="_blank">University of Wyoming Sounding Data</a></li>
	<li><a href="https://mesowest.utah.edu/" target="_blank">University of Utah MesoWest</a></li>
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
</div></div></div>
