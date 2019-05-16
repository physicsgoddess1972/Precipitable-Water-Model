## Introduction

### Goal: 

## Methodology

## Data Format

## Requirements
To satisfy the requirements to execute the script. Run ```install.sh```. 
It will install the system requirements and the R package 
requirements.

```bash
bash install.sh
```

## Using the Model

The computational model is enclosed in the script ``model.r``. 
The capabilities of the program include the production of three groups of 
plots:

 1)
 
 2)

 3)
 
For the command line arguments run the command:
```bash
Rscript model.r --help
```
There are two primary sets of plots, the first set pulls 
air and ground temperature measurements and the date
and plots the air, ground, and change in the air and ground
temperature as a time series. 
```bash
Rscript model.r --opt m
```
```bash
Rscript model.r --opt m --overcast
```
### Plot Set Contents
 1) Air Temperature Time Series

 2) Ground Temperature Time Series

 3) Change in Temperature Time Series

The second set plots the 
temperature and precipitable water. The current configuration
of ```model.r``` is set such that there are a source of 
precipitable water data for two locations at two 
different times. 
```bash
Rscript model.r --opt p
```
### Plot Set Contents
 1) Individual Location PW and Temperature
 
 2) Locational Average PW and Temperature

 3) Total Mean PW and Temperature
 
 4) Residual for Total Mean PW and Temperature  
 
## R Features
### Show and Save Functions
### Exponential Regression
### The Pac-Man Residual plot

