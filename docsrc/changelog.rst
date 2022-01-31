***********
Changelog
***********

===========
PMAT Cirrus
===========

:Version: 2.0
:Date: 6 Mar 2021
:Tagline: New and Improved PMAT

-------
Overall
-------

- [Updated] Compatible with R 4.0

----------
Data-input
----------

- [Added] Now includes relative humidity imports.
- [Added] Now pulls data from MesoWest..
- [Added] New guidelines for sensors that are not active (See Documentation Page for further info.)

------------
Setup-script
------------

- [Updated] Now installs R 4.0
- [Added] Additional argument to configure database imports (run `bash setup.sh -h` for more information)

-----
Plots
-----

- [Fixed] Fixed issues with bar charts where if there were more than three sensors, not all bar charts would be added for the remaining sensors.
- [Added] Added more time series plots and more composite plots.
- [Updated] Changed the x-axis labeling system to have tick marks at the 1st of the month.
- [Updated] Redesigned the main analytical plot, confidence interval is now a shaded region, and the plot is now monochromatic.
- [Updated] Pac-Man residual was removed from this plot set.
- [Updated] Pac-man residual now resides in a new plot set (run `Rscript model.r --pacman`)
- [Added] Mean TPW and Mean temperature comparison can now be visualized in a Pac-Man plot.

----------------
Web-applications
----------------

- [Added] Two web-apps are active. One is a Data Dashboard, which allows for the viewing of time series data as a scatter plot or a heat map, and analytical comparisons between data that has been collected.
- [Added] The Data Dashboard also allows for custom time series data to be uploaded.
- [Added] The Machine Learning dashboard now allows for custom data to be uploaded.

-------------
Documentation
-------------

- [Fixed] Fixed multiple CSS issues.
- [Updated] Altered Pac-Man residual plot documentation to refer to the package documentation
- [Updated] Updated procedure to include the new command-line arguments
- [Added] Included buttons on the dashboard's "Project Updates" card to include Pac-Man plots and Poster plots that are generated from data we have collected.
- [Updated] We also scored a `.tech` domain for the page.

----------
Automation
----------

- [Misc] This is a work in progress


================
PMAT Altocumulus
================

:Version: 1.0
:Date: 10 Nov 2019
:Tagline: Initial Deployment of The Precipitable Water Model

-------
Overall
-------

- [Added] Flexible data input mm.
- [Added] Easy Hands-off setup.
- [Added] Command-line arguments to access the different plots available
- [Added] Time Series plots for zenith sky temperature and precipitable water
- [Added] Analytical plots showing the correlation between zenith sky temperature and precipitable water
- [Added] Poster ready plots for presentations
- [Added] A data set including the average temperature and precipitable water
- [Added] The Pac-Man Residual.
- [Updated] Documentation Page.


