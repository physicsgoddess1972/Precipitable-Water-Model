---
title: 'Precipitable-Water Model Analysis Tool: An open-source suite for estimating precipitable water with low-cost instrumentation.'
tags:
  - atmospheric science
  - R
  - Python
  - Docker
authors:
  - name: Spencer Riley^[corresponding author]
    orcid: 0000-0001-7949-9163 
    affiliation: "1, 2"
  - name: Kenneth Minschwaner
    afilliation: 2
  - name: Vicki Kelsey
    affiliation: 3
affiliations:
  - name: Now at Physics Department, Montana State University, USA
    index: 1
  - name: Physics Department, New Mexico Institute of Mining and Technology, USA
    index: 2
  - name: Atmospheric and Environmental Sciences Program, South Dakota School of Mines and Technology, USA
    index: 3
date:
bibliography: paper.bib
---
# Summary
Precipitable water (PW) is the vertically integrated amount of water vapor in a column of air from the surface to the top of the atmosphere [@Salby:1996]. The Precipitable-water Model Analysis Tool (``PMAT``) is an open-source software suite designed to analyze the correlation between atmospheric brightness temperature measured at zenith (directly overhead) and precipitable water. The suite is built using R and Python programming languages and packaged via Docker.  The source code for the legacy edition of ``PMAT`` has been archived to Zenodo with linked DOI: [@pmat:2021].

# Statement of Need
Precipitable water data is important for setting initial conditions in weather forecasting models, and the accuracy and coverage (both temporal and spatial) of PW measurements greatly impact near term forecasting skill [@Marcus:2007].  In addition, long term increases in PW have been shown to be associated with warming due to increasing greenhouse gas levels in the atmosphere, leading to a positive feedback on climate [@Soden:2006]. ``PMAT`` addresses the need for an easily integrated analysis workflow to quantitatively characterize the relationship between regional PW and localized atmospheric brightness temperature observations for areas that lack high to moderate-resolution data collection infrastructure. The software suite has already been configured and deployed using data on atmospheric brightness temperature obtained from a series of infrared thermometers in the Central New Mexico climate zone [@Kelsey:2021]. The suite is well suited for community science endeavors designed to augment PW coverage. 

# Software Architecture and Design
``PMAT`` contains three primary elements: deployment, pre-processing, and analysis. The three components together can configure the container, collect and organize data, conduct primary and secondary analysis functions, then visualize the results. 

The prerequisites for the full deployment of ``PMAT`` include:

(1) A data file consisting of measurements that include date and time stamps, ground and sky temperatures, and visual condition of the sky (either "clear sky" or "overcast")

(2) A configuration file written in YAML that details sensor information, analysis parameters, and information required to retrieve data

The deployment mechanism allows end-users to implement the software suite on local and cloud-based systems, which facilitates access to the generated data products and visualizations. We have configured a template repository that illustrates the required directory structure. 
 
The pre-processing module involves aggrefating the raw data file defined in Prerequisite (1) with data from National Weather Service balloon lunches via `siphon` [@siphon] and surface dewpoint and relative humidity data from local ground stations via the University of Utah's MesoWest database. ``PMAT`` supports the usage of external data sources, such that local data files that contain precipitable water and relative humidity data may be processed. This process also includes a series of three primary filters to organize and refine the data. These filters

- separate the data into structures based on the assigned label ("clear sky" or "overcast")
- removes entries that are manually labeled Do-Not-Analyze or DNA.
- compares the standard deviation of the individual days with the average standard deviation of the dataset

The threshold of the standard deviation filter is defined by the end user's configuration file. 

The analysis module has primary and secondary functionality. First and foremost, the primary analysis function is an iterative regression algorithm that defines a correlation profile between the atmospheric brightness temperature and the precipitable water vapor content. The relationship is characterized by a two-parameter exponential equation that takes the form:
\begin{equation}
\text{PW} = Ae^{T_b B}\, .
\end{equation}
The result of this process is two output files. One is for the results at each step, and the other is the final averaged regression statistics. The plots associated with the analysis process will utilize the averaged coefficients and statistics.

The secondary analysis functions consist of climatological modeling, time series analysis, and a linear support vector machine. The climatological analysis computes the average value on the same day of the year. The time series analysis includes the computation of regression models for sinusoidal data. Finally, the included linear support vector machine employs the ``e1071`` R package [@e1071] to classify atmospheric brightness temperature and precipitable water data into the aforementioned label scheme.

At the conclusion of this workflow, the end-users have access to a collection of generated data files, plot sets, and analysis results.

# Acknowledgements

# References
