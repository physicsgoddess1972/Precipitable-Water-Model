***************
Getting Started
***************

============
Introduction 
============

The Precipitable-water Model Analysis Tool (``PMAT``) is a computational utility that is used to analyze the data collected from this project to understand the relationship between the zenith sky temperature and precipitable water in the atmosphere. ``PMAT`` has three different modules that work together to present data.

The first is the Deployment Module. This module acts as the user interface for the software suite, whether it be locally or through cloud services.

The second is the Pre-processing Module, this module imports data from University of Utah's MesoWest and the University of Wyoming UpperAir Databases. 

The third module is the main program to run the analysis, the DAnalysis Module. Here the all of the data is presented and the regression analysis between precipitable water and zenith sky temperature is conducted.

====================================
Installation and Deployment Tutorial
====================================

We also require two data files. One that contains the raw data collected by the temperature sensors, that also includes date and time information (``cool_data.csv``). The second should contain sensor definitions with additional parameters for the preprocessing and analysis phases (``_pmat.yml``). A template and detailed breakdown of the configuration file is provided in Chapter 2.1, followed by a detailed breakdown on the data file format.

Github
------
This version of the Deployment module is, for the most part, automated and recommended. Follow the steps in this section to successfully deploy PMAT through GitHub with GitHub Actions.

1. Create a GitHub repository from the `template repository <https://template.pmat.app>`_. 
2. Edit the README.md page based on your location and username
3. Update all files that are contained in the ``data/`` directory, and utilize the documentation on data formatting that is provided
4. Upon finializing updates on ``cool_data.csv``, the workflow will run automatically and the visual and data products will be generated

Amazon Web Service (AWS)
------------------------
For Amazon Web Services, ``PMAT`` can be configured through the EC2 virtual machines. Once they have been configured, connect to the virutal machine.
Once connected, enter the following commands

::

   sudo yum update -y

::

  sudo amazon-linux-extras install docker

::
  
  sudo docker pull ghcr.io/physicsgoddess1972/pmat:latest
 
From here, data files can be added and utilizing the ``local_deploy.sh`` script, ``PMAT`` can be executed. 

Google Cloud Console (GCloud)
-----------------------------

Local
-----
We fully support Ubuntu and Debian systems. We do have minimal Windows support through the usage of Windows Subsystem for Linux (WSL) and Virutalization.


Development
-----------



