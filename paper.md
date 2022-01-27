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
affiliations:
  - name: Physics Department, New Mexico Institute of Mining and Technology
    index: 1
  - name:  Atmospheric and Environmental Sciences Program, South Dakota School of Mines and Technology
    index: 2
date:
bibliography: paper.bib
---
# Summary
The Precipitable-Water Model Analysis Tool (``PMAT``) is an open-source software suite designed to study the correlation between atmospheric brightness temperature and precipitable water (PWAT) data. PMAT addresses the need for an easily integrated analysis workflow to quantitatively characterize the relationship between regional PWAT and localized atmospheric brightness temperature observations for areas that lack high to moderate-resolution data collection infrastructure. The workflow contains three primary modules: deployment, pre-processing, and analysis. The deployment mechanism allows users to implement the software suite on local and cloud-based systems, which facilitates access to the generated data products and visualizations. The pre-processing stage involves aggregating atmospheric brightness temperature data collected in the field with data from radiosondes and local ground stations. The final element consists of an iterative algorithm that generates an average regression model from the collected data. This model allows for the estimation of PWAT through measured atmospheric brightness temperature. ``PMAT`` has already been configured and deployed for use in the South-Central New Mexico climate zone using a series of handheld infrared thermometers as the source of atmospheric brightness temperature data. We plan to expand the scope of the research with community science endeavors in South Dakota; while also expanding the suite’s analysis capabilities with machine learning and additional statistical products for predictive modeling. 

# Statement of Need



# Software Architecture and Design
There are currently three primary modules: Deployment, Pre-processing, and Analysis. Collectively, the three modules can pre-process data collected in the field, process the data, conduct analysis functions, and then visualize the results [@pmat].

The two prerequisites for the full deployment of `PMAT` are:

(1) A data file consisting of measurements that include ground and sky temperatures in addition to time and date stamps and the visual condition of the sky (either clear sky or overcast)

(2) A configuration file that details the sensor information, analysis parameters, and information needed to retrieve data from atmospheric databases.

The Deployment module utilizes a Docker environment to run the remaining workflow on cloud-based infrastructure and local systems. We have configured a template repository \footnote{https://template.pmat.app} for GitHub. This pre-built repository has the correct directory arrangement and the raw data and configuration files.

The Pre-processing module consists of an algorithm to acquire and organize data. The core of this module is a Python script that will first read in the raw data file and collect atmospheric data from the University of Wyoming Upper-Air database via `siphon` and the University of Utah's MesoWest database. `PMAT` also supports the usage of external data sources, so local data files that contain precipitable water and relative humidity data can be processed in this section.

The primary role of the Analysis module is to present the correlation between PWAT and zenith sky temperature in terms of regression statistics. In the `PMAT` analysis methodology, we have implemented an iterative process for computing the exponential regression statistics and coefficients. The PWAT and temperature data undergo a linearization in the form of the exponential equation:

\begin{equation}\text{PWAT} = a e^{b T_b} . \end{equation}

This process includes a data partition function and a standard deviation-based filter to minimize measurement errors. Secondary analysis functions include sinusoidal curve-fitting on time-series data and the implementation of moving-average overlays on time-series plots. 

# Future Developments 
As we continue to develop and maintain the `PMAT` software, we intend to add additional modules and optional analytical functions. One of the major developments is a machine learning module that utilizes TensorFlow's support vector machine framework to classify the dataset into a binary weather condition scheme. 

# Acknowledgements

# References
