---
title: 'DRAFT: Precipitable-Water Model Analysis Tool: An open-source suite for estimating precipitable water with low-cost instrumentation.'
tags:
  - atmospheric science
  - R
  - Python
  - Docker
authors:
  - name: Spencer Riley^[corresponding author]
    orcid: 0000-0001-7949-9163 
    affiliation: 1
  - name: Vicki Kelsey^[co-first author]
    affiliation: 2
  - name: Kenneth Minschwaner
    afilliation: 1
affiliations:
  - name: Physics Department, New Mexico Institute of Mining and Technology
    index: 1
  - name:  Atmospheric and Environmental Sciences Program, South Dakota School of Mines and Technology
    index: 2
date:
bibliography: paper.bib
---
# Summary
Precipitable water is the vertically integrated amount of water vapor in a column of air from the surface to the top of the atmosphere [@Salby:1996]. The Precipitable-water Model Analysis Tool (``PMAT``) is an open-source software suite designed to analyze the correlation between atmospheric brightness temperature and precipitable water. The suite is built using R and Python programming languages and packaged via Docker.  The source code for the legacy edition of ``PMAT`` has been archived to Zenodo with linked DOI: [@pmat].

# Statement of Need
Precipitable water data can be used in weather forecasting to determine the amount of potential rainfall available and to study the dynamical evolution of convective storms. The lack of reliable and sufficient precipitable water data can have a significant impact on the quality of weather forecasts [@Yang:2018,@Smith:2007]. ``PMAT`` addresses the need for an easily integrated analysis workflow to quantitatively characterize the relationship between regional PWAT and localized atmospheric brightness temperature observations for areas that lack high to moderate-resolution data collection infrastructure. The software suite has already been configured and deployed for use in the South-Central New Mexico climate zone using a series of handheld infrared thermometers as the source of atmospheric brightness temperature data [@Kelsey:2022]. As we plan to expand the scope of atmospheric science research, we aim to incorporate community science endeavors in South Dakota.

# Software Architecture and Design
``PMAT`` contains three primary elements: deployment, pre-processing, and analysis. The three components can configure the container, collect and organize data, conduct primary and secondary analysis functions, then visualize the results. 

There are two primary prerequisites for the full deployment of ``PMAT``:

(1) A data file consisting of measurements that include ground and sky temperatures, time and date stamps, and visual condition of the sky (either clear-sky or overcast)

(2) A configuration file written in YAML that details sensor information, analysis parameters, and information required to retrieve data

The deployment mechanism allows end-users to implement the software suite on local and cloud-based systems, which facilitates access to the generated data products and visualizations. We have configured a template repository that illustrates the required directory structure. 
 
The pre-processing module involves aggregating atmospheric brightness temperature data collected in the field with data from radiosondes and local ground stations. The core of this subprocess is a Python script that will read in the raw data and collect atmospheric data from the University of Wyoming Upper-Air database via ``siphon`` [siphon] in addition to the University of Utah MesoWest database. ``PMAT`` supports the usage of external data sources, such that local data files that contain precipitable water and relative humidity data may be processed. This process also includes a series of three primary filters to organize and refine the data. These filters

- separate the data into structures based on the assigned label (overcast or clear sky)
- removes entries that are manually labeled Do-Not-Analyze or DNA.
- compares the standard deviation of the individual days with the average standard deviation of the dataset

The threshold of the standard deviation filter is defined by the end user's configuration file. 

The analysis module has primary and secondary functionality. First and foremost, the primary analysis function is an iterative regression algorithm that defines a correlation profile between the atmospheric brightness temperature and the precipitable water vapor content. The relationship is characterized by a two-parameter exponential equation that takes the form:
\begin{equation}
PWAT = Ae^{T_b B}\, .
\end{equation}
The result of this process is two output files. One is for the results at each step, and the other is the final averaged regression statistics. The plots associated with the analysis process will utilize the averaged coefficients and statistics.

The secondary analysis functions consist of climatological modeling, time series analysis, and a linear support vector machine. The climatological analysis computes the average value on the same day of the year. The time series analysis includes the computation of regression models for sinusoidal data. This feature is specifically applied to the atmospheric brightness temperature observations as a means to extrapolate the corresponding precipitable water vapor content. Finally, the included linear support vector machine employs the ``e1071`` R package [@e1071] to classify atmospheric brightness temperature and precipitable water data into the aforementioned label scheme (overcast and clear sky).

At the conclusion of this workflow, the end-users have access to a collection of generated data files, plot sets, and analysis results.

# Acknowledgements

# References
