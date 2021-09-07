---
title: 'Precipitable-Water Model Analysis Tool'
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
## This is a draft
# Summary

The Precipitable-water Model Analysis Tool (PMAT) utilizes four distinct modules to operate: Deployment, Data Import, Data Analysis, and the Classical Support Vector Machine module. Together the four modules can pre-process data collected in the field and aggregate with selected atmospheric science databases, then process the data, conduct primary analysis functions, and then visualize the results [@pmat].

Precipitable water can be described as the ... . 
The Precipitable-Water Model Analysis Tool (PMAT) is a utility designed to analyze the relationship between zenith clear sky temperature and precipitable water vapor. This relationship has been well documented and defined in Kelsey_2021 and Mims_2013. 

# Statement of Need
PMAT has been developed to address our own need for an analysis tool for studying the relationship between zenith sky temperature and total precipitable water.

# Software Architecture and Design
The PMAT system is designed into modules, each of the different modules handles a unique task. There are currently four primary modules:  the Deployment, Data Import, Data Analysis, and Classical Support Vector Machine module. 

Prerequisites for the full deployment of PMAT are minimal but do exist. There are two major requirements that need to be fulfilled:

(1) a data file consisting of manual (or automated) measurements that include ground and sky temperatures in addition to time and data stamps and the visual condition of the sky (either clear sky or overcast)

(2) a configuration file that details the sensor information and the site IDs that are needed to retrieve the atmospheric data from the NOAA database

The Deployment module utilizes a docker environment to run the remaining workflow. 
There are two variations of the Deployment module, the first is the GitHub Interface, where ... . 
The second is the local interface, in this variation the docker container can be pulled from the GitHub Container Registry and continuously run in a local environment. 

The Data Import module consists of a Python script to pull and organize data that are required to complete the analysis. The Python script


# Acknowledgements

# References
