# Introduction
The intent of this document is to throuoghly describe how to utilize the legacy version of the PMAT software suite. This will be a definitive and authorative document on the legacy software, any additional questions can be sent to the primary maintainer, Spencer Riley.

## Requirements
As of the release of the legacy suite, we fully support Ubuntu and Debian systems. We do have minimal Windows support through the usage of Windows Subsystem for Linux (WSL) and Virutalization.

We also require two data files. One that contains the raw data collected by the temperature sensors, that also includes date and time information (`cool_data.csv`). The second should contain sensor definitions with additional parameters for the preprocessing and analysis phases (`_pmat.yml`). A template and detailed breakdown of the configuration file is provided in Chapter 2.1, followed by a detailed breakdown on the data file format.

## What is PMAT?
The Precipitable-Water Model Analysis Tool (PMAT) is a computational utility that is used to analyze the data collected from temperature sensors to understand the relationship between the zenith sky temperature and precipitable water in the atmosphere. PMAT has 3 different modules that work together to present data.

The first is the Deployment Module. This module acts as the user interface for the software suite, whether it be locally or through GitHub. The second is the Preprocessing Module, this module imports data from MesoWest and the University of Wyoming UpperAir Databases. This part relies on the information in the import.conf file and the accompanying Import Configuration Dashboard which is currently under development. The third module is the Data Analysis Module. Here the all the data is presented and the regression analysis between precipitable water and zenith sky temperature is conducted.


