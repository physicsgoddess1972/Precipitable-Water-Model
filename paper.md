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
In this paper we will adapt a common definition of precipitable water vapor (PWV) as defined by (get citation) which states that PWV is the integrated amount of water vapor for a vertical column of air with height from the surface to the top of the atmosphere. The Precipitable-Water Model Analysis Tool (PMAT) is a utility designed to analyze the relationship between zenith clear sky temperature and precipitable water vapor. This relationship has been well documented and defined in both Central New Mexico [@Kelsey:2021] and Eastern Texas [@Mims:2011]. 

# Statement of Need
PMAT has been developed to address the need for an easy to use workflow to analyze the relationship between PWV and zenith sky temperature. This software can be used to more easily analyze measurements of th

# Software Architecture and Design
The PMAT system is designed into modules, each of the different modules handles a unique task. There are currently four primary modules: Deployment, Data Import, Data Analysis, and the Classical Support Vector Machine module. Together the four modules can pre-process data collected in the field and aggregate with selected atmospheric science databases, process the data, conduct primary analysis functions, and then visualize the results [@pmat].

Prerequisites for the full deployment of PMAT are minimal but do exist. There are two major requirements that need to be fulfilled:

(1) a data file consisting of manual (or automated) measurements that include ground and sky temperatures in addition to time and date stamps and the visual condition of the sky (either clear sky or overcast)

(2) a configuration file that details the sensor information and the site IDs that are needed to retrieve the atmospheric data from the NOAA database

## Deployment Module
The Deployment module utilizes a docker environment to run the remaining workflow. There are currently two methods to interface with PMAT through the Deployment Module, the first is through GitHub and the second involves a local installation of Docker. 

## Data Import Module
The Data Import module consists of a Python script to pull and organize data that are required to complete the analysis. The Python script

## Data Analysis Module
The function of the analysis component of PMAT is to present the relationship between PWV and zenith sky temperature in term of statistics and regression. The PWV and temperature subsets of the data undergo a linearization in the form of 
log(PWV) = log(A) + BT_b,
## Classical Support Vector Machine Module

# Acknowledgements

# References
