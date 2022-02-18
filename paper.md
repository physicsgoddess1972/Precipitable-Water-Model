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
Precipitable water is the vertically integrated amount of water vapor in a column of air from the surface to the top of the atmosphere [@Salby:1996]. The Precipitable-water Model Analysis Tool (``PMAT``) is an open-source software suite designed to study the correlation between atmospheric brightness temperature and precipitable water [@pmat:2021]. 

# Statement of Need
Precipitable water data can be used in weather forecasting to determine the amount of potential rainfall available and to study the dynamical evolution of convective storms. The lack of reliable and sufficient precipitable water data can have a significant impact on the quality of weather forecasts [@Yang:2018, @Smith:2008]. ``PMAT`` addresses the need for an easily integrated analysis workflow to quantitatively characterize the relationship between regional PWAT and localized atmospheric brightness temperature observations for areas that lack high to moderate-resolution data collection infrastructure. The software suite has already been configured and deployed for use in the South-Central New Mexico climate zone using a series of handheld infrared thermometers as the source of atmospheric brightness temperature data. We plan to expand the scope of the research with community science endeavors in South Dakota; while also expanding the suite’s analysis capabilities with machine learning and additional statistical products for predictive modeling. 

# Software Architecture and Design
``PMAT`` contains three primary modules: deployment, pre-processing, and analysis. The three modules can collect and organize data, conduct primary and secondary analysis functions, then visualize the results. 

There are two primary prerequisites for the full deployment of ``PMAT``:

(1) A data file consisting of measurements that include ground and sky temperatures, time and date stamps, and visual condition of the sky (either clear-sky or overcast)

(2) A configuration file that details sensor information, analysis parameters, and information required to retrieve data

The deployment mechanism allows end-users to implement the software suite on local and cloud-based systems, which facilitates access to the generated data products and visualizations. We have configured a template repository that illustrates the required directory structure. 
 
The pre-processing module involves aggregating atmospheric brightness temperature data collected in the field with data from radiosondes and local ground stations. The core of this subprocess is a Python script that will read in the raw data and collect atmospheric data from the University of Wyoming Upper-Air database via ``siphon`` and the University of Utah MesoWest database. ``PMAT`` supports the usage of external data sources, such that local data files that contain precipitable water and relative humidity data be processed. This process also includes a series of three primary filters to organize and refine the data. The first filter separates the labeled overcast and clear-sky data. Next, the data which contains the label Do Not Analyze (DNA) is filtered out. This DNA label is added manually to the raw data file and can be used to remove outliers. The last filter compared the standard deviation of the individual days with the average standard deviation of the dataset. The threshold of this filter originates from the end-user's configuration file. 


The primary role of the Analysis module is to present the correlation between PWAT and zenith sky temperature in terms of regression statistics. In the ``PMAT`` analysis methodology, we have implemented an iterative process for computing the exponential regression statistics and coefficients. The PWAT and temperature data undergo a linearization in the form of the exponential equation:

\begin{equation}\text{PWAT} = A e^{B T_b} . \end{equation}

After the iteratation process is complete, the regression statistics are averaged together to further minimize random bias. The results of the individual step runs and the final averages are presented as a part of the data products that are generated. In addition, we have secondary analysis functions which include instrument comparisons and sinusodal regression for time-series data. 

# Future Developments 
As we continue to develop and maintain the ``PMAT`` software, we intend to add additional modules and optional analytical functions. One of the major developments is a machine learning module that utilizes TensorFlow's support vector machine framework to classify the dataset into a binary weather condition scheme. 

# Acknowledgements

# References
