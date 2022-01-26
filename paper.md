---
title: 'Precipitable-Water Model Analysis Tool: An open source package for estimating precipitable water'
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
The Precipitable-Water Model Analysis Tool (PMAT) is an open-source software suite designed to study the correlation between atmospheric brightness temperature and precipitable water (PWAT) data. The workflow contains three primary modules: deployment, pre-processing, and analysis. The deployment mechanism allows users to implement the software suite on local and cloud-based systems, which facilitates access to the generated data products and visualizations. The pre-processing stage involves aggregating atmospheric brightness temperature data collected in the field with data from radiosondes and local ground stations. The final element consists of an iterative algorithm that generates an average regression model from the collected data. This model allows for the estimation of PWAT through measured atmospheric brightness temperature. PMAT has already been configured and deployed for use in the South-Central New Mexico climate zone using a series of handheld infrared thermometers as the source of atmospheric brightness temperature data. We plan to expand the scope of the research with community science endeavors in South Dakota; while also expanding the suite’s analysis capabilities with machine learning and additional statistical products for predictive modeling.
# Statement of Need
PMAT addresses the need for an easily integrated analysis workflow to quantitatively characterize the relationship between regional PWAT and localized atmospheric brightness temperature observations for areas that lack high to moderate-resolution data collection infrastructure. 

# Software Architecture and Design
The PMAT suite contains a series of each modules, each handles  There are currently four primary modules: Deployment, Preprocessing, and Analysis module. Together the three modules can pre-process data collected in the field and aggregate with selected atmospheric science databases, process the data, conduct primary analysis functions, and then visualize the results [@pmat].

Prerequisites for the full deployment of PMAT are minimal but do exist. There are two major requirements that need to be fulfilled:

(1) A data file consisting of measurements that include ground and sky temperatures in addition to time and date stamps and the visual condition of the sky (either clear sky or overcast)

(2) A configuration file that details the sensor information, site IDs that are needed to retrieve the atmospheric data from the NOAA database, and analysis parameters. 

The Deployment module utilizes a Docker environment to run the remaining workflow. Currently, there are two methods to interface with PMAT; through GitHub and locally. The GitHub deployment method can be configured automatically with a template repository\footnote{https:template.pmat.docs}. The pre-built GitHub repository has the correct directory arrangement and the raw data and configuration files.

The Preprocessing module consists of an algorithm to acquire and organize data. The core of this module is a Python script that will first read in the raw data file and collect atmospheric data from the University of Wyoming Upper-Air database via `siphon` and the University of Utah's MesoWest database. PMAT also supports the usage of external data sources, so local data files that contain precipitable water and relative humidity data can be processed in this section.

The primary role of the Analysis module is to present the correlation between PWV and zenith sky temperature in terms of regression statistics. In the PMAT analysis methodology, we have implemented an iterative process for computing the exponential regression statistics and coefficients. This process includes a data partition function and a standard deviation-based filter to minimize measurement errors. The PWV and temperature data undergo a linearization in the form of the exponential equation:

\begin{equation}\text{PWV} = a e^{b T_b} . \end{equation}

The software products that PMAT generates include a variety of time series, instrument-specific, and comparative analysis plot sets, in addition to raw data products.  

# Future Developments 
As we continue to develop and maintain the PMAT software, we intend to add additional modules and optional analytical functions. One of the major developments is a machine learning module that utilizes TensorFlow's support vector machine framework to classify the dataset into a binary weather condition scheme. 

# Acknowledgements

# References
