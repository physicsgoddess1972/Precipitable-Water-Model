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
	<td>Create a GitHub repository from the <a href="https://github.com/PharaohCola13/pmat-template">template</a>.</td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
	<td>Edit the README.md page based on your location and username</td>
</tr>
<tr>
	<td><span class="numbered">3</span></td>
	<td>Update all files that are contained in the <code>data/</code> directory, and utilize the <a href="https://physicsgoddess1972.github.io/Precipitable-Water-Model/index.html#data-format">documentation on data formatting.</a></td>
</tr>
<tr>
	<td><span class="numbered">4</span></td>
	<td>Upon finializing updates on <code>cool_data.csv</code>, the workflow will run and the plots and master dataset will be generated.</td>
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
  <td>Download the bash script from the <code>util/local/</code> directory, <a href="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/master/util/local/pmat.sh">here</a>.</td>
</tr>
<tr>
	<td><span class="numbered">2</span></td>
  <td>Download the template repository from <a href="https://github.com/PharaohCola13/pmat-template">here</a></td>
</tr>
<tr>
	<td><span class="numbered">3</span></td>
	<td>Create and fill out the configuration files: <code>_pmat.yml</code>.</td>
</tr>
<tr>
	<td><span class="numbered">4</span></td>
  <td>Run the command <code>bash pmat.sh -i</code></td>
</tr>
<tr>
    <td><span class="numbered">5</span></td>
    <td>Input all data collected into <code>data/cool_data.csv</code></td>
</tr>
<tr>
    <td><span class="numbered">6</span></td>
  <td>To execute the PMAT suite for the data, run the command <code>bash pmat.sh -R</code></td>
</tr>
</tbody>
</table>
