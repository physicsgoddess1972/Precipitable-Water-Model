<a id="top"></a>
<div class="section timeline">
  <div class='timeline-item'>
    <div class="content">
      <div class="collapsible">
            <div class="collapsible-header">
                <h2>PMAT v2.0 (Cirrus) <span class="label label-rounded text-light text-capitalize tag-date">TBA</span></h2></div>
            <div class="panel">
              <h4 style="color:black">New and Improved PMAT</h4>
              <div>
                <h3>Overall</h3>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Compatible with R 4.0
                  </li>
                <h3>Data input</h3>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                    Now includes relative humidity imports</li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                      Now pulls data from MesoWest</li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                    New guidelines for sensors that are not active (See Documentation Page for further info.)</li><br>
                <h3>Setup Script</h3>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                      Now installs R 4.0</li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                    Additional argument to configure database imports (run `bash setup.sh -h` for more information)</li><br>
                <h3>Plots</h3>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Minor tick marks have been added to all applicable plots</li><br>
                  <b>Poster plots</b><br><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Fixed issues with bar charts where if there were more than three sensors, not all bar charts would be added for the remaining sensors.</li><br>
                  <b>Time Series plots</b><br><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                    Added more time series plots and more composite plots.</li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Changed the x-axis labeling system to have tick marks at the 1st of the month.</li><br>
                  <b>Analytical plots</b><br><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Redesigned the main analytical plot, confidence interval is now a shaded region, and the plot is now monochromatic.</li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Pac-Man residual was removed from this plot set</li><br>
                  <b>Pac-Man plots</b><br><br>
                  <li style="list-style: none;">
                  <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Pac-man residual now resides in a new plot set (run `Rscript model.r --pacman`)
                  </li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                    Mean TPW and Mean temperature comparison can now be visualized in a Pac-Man plot.
                  <li>
                <h3>Web Applications</h3>
                  <li style="list-style: none;">
                  <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  Two web-apps are active. One is a Data Dashboard, which allows for the viewing of time series data as a scatter plot or a heat map, and analytical comparisons between data that has been collected.
                  </li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                    The Data Dashboard also allows for custom time series data to be uploaded.
                  </li><br>
                <h3>Documentation</h3>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Fixed multiple CSS issues
                  </li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Altered Pac-Man residual plot documentation to refer to the package documentation
                  </li><br>
                  <li style="list-style: none;">
                  <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    Updated procedure to include the new command-line arguments
                  </li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                    Included buttons on the dashboard's "Project Updates" card to include Pac-Man plots and Poster plots that are generated from data we have collected.
                  </li><br>
                  <li style="list-style: none;">
                    <span class="label label-rounded text-light text-capitalize tag-changed">Updated</span>
                    We also scored a `.tech` domain for the page
                  </li>
                <h3>Automation Project</h3>
                  <li style="list-style: none;">
                    This is a work in progress
                  </li></div></div>
          </div></div>
    </div>
  </div>
  <div class="timeline-item">
    <div class="content">
      <div class="collapsible">
        <div class="collapsible-header">
            <h2>PMAT v1.0 (Altocumulus) <span class="label label-rounded text-light text-capitalize tag-date">11/10/2019</span></h2></div>
          <div class="panel">
              <h4 style="color:black">Initial Deployment of The Precipitable Water Model</h4><div>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  Flexible data input
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  Easy, Hands-off setup
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  Command-line arguments to access the different plots available
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  Time Series plots for zenith sky temperature and precipitable water
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  Analytical plots showing the correlation between zenith sky temperature and precipitable water
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  Poster ready plots for presentations
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  A data set including the average temperature and precipitable water
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-added">Added</span>
                  The Pac-Man Residual.
              </li><br>
              <li style="list-style: none;">
                <span class="label label-rounded text-light text-capitalize tag-changed">In Progress</span>
                  Documentation Page
              </li><br></div>
        </div>
      </div>
  </div>
</div>
