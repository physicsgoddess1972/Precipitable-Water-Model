**************
PMAT Reference
**************


---------------
pmat_analysis.r
---------------
	:module: Precipitable Water Model Analysis Tool: Analysis
	:synopsis: This module contains analysis functions

.. function:: exp.regression(t=NULL,mean.out)

	:detail: Function includes all of the stuff to generate the exponential regression model with intervals
	:param double t: training fraction
	:param list mean.out: the output of mean.filter
	:return: returns the data series and model statistics
	:rtype: list

.. function:: lin.regression(x,y)

	:detail: Linear regression function
	:param double x: the domain of the dataset
	:param double y: the range of the dataset
	:return: returns the data series and model statistics
	:rtype: list

.. function:: data.partition(x,y,tr.sz=tr.sz)

	:detail: splits the data into a training/testing set
	:param double x: domain of the data
	:param double y: range of the data
	:param double tr.sz: fraction of the data in the testing set
	:return: a list containing the training and testing sets
	:rtype: list

.. function:: iterative.analysis(out.bool,mean.out)

	:detail: computes regression statistics and outputs to a yaml file
	:param logical out.bool: determine whether to generate new _output.yml
	:param list mean.out: output of mean.filter
	:return: iterative stats and _output.yml
	:rtype: list

.. function:: lsvm(x,y,l,tr.sz=0.7,seed=sample(1:2^15,1))

	:detail: Generates a Linear Support Vector Machine and draws the decision hyperplane and support vectors
	:param double x: domain of dataset
	:param double y: range of dataset
	:param double l: labels of the dataset
	:param double tr.sz: fraction of data to be used for model training
	:param integer seed: the random seed
	:return: list of data, labels, and the coefficients
	:rtype: list

-----------------
pmat_processing.r
-----------------
	:module: Precipitable Water Model Analysis Tool: Pre-processing
	:synopsis: functions for preprocessing

.. function:: colscheme(range,set="Set1")

	:detail: a function that generates an array of colors based on the number of elements
	:param list range: a list of data series
	:return: a list of colors
	:rtype: list

.. function:: nan.filter(stuff)

	:detail: removes nan values from a set of lists
	:param list stuff: list of arrays
	:return: returns list with filtered data and the indices with nans
	:rtype: list

.. function:: mean.filter(nan.out,n)

	:detail: filters the data based on the comparison of the daily std and the average std of the dataset
	:param list nan.out: the output of nan.filter
	:param integer n: threshold
	:return: an array of indices for PWV values to be analyzed
	:rtype: list

.. function:: dna.filter(filter.res)

	:detail: removes data labels as Do Not Analyze
	:param list filter.res: overcast.filter results
	:return: overcast.filter results with DNA points removed
	:rtype: list

.. function:: inf.counter(bool,snsr_data,label)

	:detail: identifies the -Inf values
	:param logical bool: decides if -Inf is not replaced with NaN
	:param list snsr_data: the dataset
	:param character label: the identifier for the dataset (e.g. sky, gro, skyo, groo)
	:return: data set that replaces all -Infs for NaN (If bool == FALSE).
	:rtype: list

.. function:: index.norm(x)

	:detail: calculates the normalized index of the dataset
	:param double x: data range
	:return: an array of values between 0 and 1
	:rtype: double

.. function:: overcast.filter(col_con,col_date,col_com,pw_name,snsr_name,cloud.bool)

	:detail: Filters our data with overcast condition
	:param integer col_con: column index for condition labels
	:param integer col_date: column index for date stamp
	:param integer col_com: column index for comments
	:param list pw_name: pw measurement labels
	:param list snsr_name: sensor labels
	:param logical cloud_bool:
	:return: A list of lists containing either clear-sky/overcast data
	:rtype: list

.. function:: sky.processing(filter.res)

	:detail: Computes average values and weighted averages
	:param list filter.res: results of the overcast.filter function
	:return: series of arrays including average PWV, RH, etc.
	:rtype: list

---------------
pmat_products.r
---------------
	:module: Precipitable Water Model Analysis Tool: Products
	:synopsis: plotting functions for PMAT

.. function:: time.pwindex(datetime)

	:detail: Normalized PWV index for both clear sky and overcast data
	:param datetime: the datestamp of the data

.. function:: time.nth_range(y,title,color,leg.lab,ylab,datetime,over.bool)

	:detail: Multirange Time Series plot series
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character color: the color string for the data range
	:param list leg.lab: a list of
	:param character ylab: the y-axis label
	:param double datetime: the datetime array
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: time.composite(y,title,color,ylab,datetime,over.bool)

	:detail: Time Series composite plot series
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character color: the color string for the data range
	:param character ylab: the y-axis label
	:param double datetime: the datetime array
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: time.mono_composite(y,title,ylab,datetime,over.bool)

	:detail: Time Series composite plot series
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character ylab: the y-axis label
	:param double datetime: the datetime array
	:param logical overcast: the condition of data (clear sky/overcast)

.. function:: time.multiyear(y,title,color,datetime,ylab,over.bool)

	:detail: Climatology plot
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character color: the color string for the data range
	:param double datetime: the datetime array
	:param character ylab: the y-axis label
	:param logical overcast: the condition of data (clear sky/overcast)

.. function:: analysis.nth_range(over.bool,x,y,title,label,color,leg.lab)

	:detail: Comparative analysis plot
	:param logical overcast: the condition of data (clear sky/overcast)
	:param x: the domain of the plot
	:param y: the range of the plot
	:param title: the title/description of the plot
	:param label:
	:param color: the color string for the data
	:param leg.lab:

.. function:: analysis.regression(over.bool,x,y,title,label,iter)

	:detail: Super Average Plot with Exponential Fit
	:param bool overcast: the condition of data (clear sky/overcast)
	:param double x: the domain of the plot
	:param double y: the range of the plot
	:param character title: the title/description of the plot
	:param list label: list containing the x-axis and y-axis labels
	:param list iter: the output of iter.analysis

.. function:: analysis.svm(model)

	:detail: plots SVM
	:param list model: output of lsvm

.. function:: pac.compare(over.bool,title,x,y,angular,radial)

	:detail: Pac-Man plot of Super Average Plot
	:param logical overcast: the condition of data (clear sky/overcast)
	:param character title: the title/description of the plot
	:param double x: the domain of the plot
	:param double y: the range of the plot
	:param character angular: the angular axis label and units
	:param character radial: the radial axis label and units

.. function:: pac.regression(over.bool)

	:detail: Pac-Man residual plot
	:param bool over.bool: the condition of data (clear sky/overcast)

.. function:: chart.histogram(range,xlabel,title)

	:detail: Histograms of defined quantities
	:param double range: the range of the plot
	:param list xlabel: the x-axis label of the plot
	:param list title: the title/description of the histogram

.. function:: poster.plots(over.bool,iter,mean.out)

	:detail: The set of all poster
	:param bool overcast: the condition of data (clear sky/overcast)
	:param list iter: output of iterative.analysis
	:param list mean.out:
	:return: All available poster plots

.. function:: poster1(...)


.. function:: poster2(over.bool,iter,mean.out)

	:detail: The analytics poster plot
	:param logical overcast: the condition of data (clear sky/overcast)
	:param iter: the output of iterative.analysis
	:param mean.out:

.. function:: sensor.chart(...)

	:detail: sensor specific data type distribution charts

.. function:: sensor.time(over.bool)

	:detail: Instrumentation time series plots
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: data.gen(over.bool,dir)

	:detail: creates a datafile containing the date, avg temp, and avg pwv for a defined condition
	:param bool over.bool: the condition of the data (clear sky/overcast)
	:param dir: directory path

.. function:: data.ml(out.dir)

	:detail: creates a datafile containing the machine learning relavant information
	:param character out.dir: directory path

.. function:: data.step(seed,i,coef,r,S)

	:detail: writes a yaml object that contains the analysis results of each step in the iterative analysis
	:param integer seed: the generated seed
	:param integer i:  the step number
	:param list coef: the coefficients of the best-fit
	:param double r: the RSME value
	:param double S: the standard deviation
	:return: a yaml object
	:rtype: list

.. function:: data.final(out.dir,lengths,frac.kept,coef,std,rmse,over.bool=args$overcast)

	:detail: writes the final results of iterative.analysi
	:param character out.dir: the output directory
	:param list lengths: list of lengths
	:param list coef: the average coefficients of the best-fit
	:param double std: the average standard deviation
	:param double rmse: the average rsme values
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: visual.products(set,mean.out,datetime=datetime,over.bool=args$overcast)

	:detail: saves plot sets
	:param character set: the --set parameter
	:param mean.out: output of mean.filter
	:param datetime: the datetime range of the data
	:param logical over.bool: the condition of data (clear sky/overcast)
	:module: Precipitable Water Model Analysis Tool: Products
	:synopsis: plotting functions for PMAT

.. function:: time.pwindex(datetime)

	:detail: Normalized PWV index for both clear sky and overcast data
	:param datetime: the datestamp of the data

.. function:: time.nth_range(y,title,color,leg.lab,ylab,datetime,over.bool)

	:detail: Multirange Time Series plot series
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character color: the color string for the data range
	:param list leg.lab: a list of
	:param character ylab: the y-axis label
	:param double datetime: the datetime array
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: time.composite(y,title,color,ylab,datetime,over.bool)

	:detail: Time Series composite plot series
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character color: the color string for the data range
	:param character ylab: the y-axis label
	:param double datetime: the datetime array
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: time.mono_composite(y,title,ylab,datetime,over.bool)

	:detail: Time Series composite plot series
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character ylab: the y-axis label
	:param double datetime: the datetime array
	:param logical overcast: the condition of data (clear sky/overcast)

.. function:: time.multiyear(y,title,color,datetime,ylab,over.bool)

	:detail: Climatology plot
	:param list y: the range of the plot
	:param character title: the title/description of the plot
	:param character color: the color string for the data range
	:param double datetime: the datetime array
	:param character ylab: the y-axis label
	:param logical overcast: the condition of data (clear sky/overcast)

.. function:: analysis.nth_range(over.bool,x,y,title,label,color,leg.lab)

	:detail: Comparative analysis plot
	:param logical overcast: the condition of data (clear sky/overcast)
	:param x: the domain of the plot
	:param y: the range of the plot
	:param title: the title/description of the plot
	:param label:
	:param color: the color string for the data
	:param leg.lab:

.. function:: analysis.regression(over.bool,x,y,title,label,iter)

	:detail: Super Average Plot with Exponential Fit
	:param bool overcast: the condition of data (clear sky/overcast)
	:param double x: the domain of the plot
	:param double y: the range of the plot
	:param character title: the title/description of the plot
	:param list label: list containing the x-axis and y-axis labels
	:param list iter: the output of iter.analysis

.. function:: analysis.svm(model)

	:detail: plots SVM
	:param list model: output of lsvm

.. function:: pac.compare(over.bool,title,x,y,angular,radial)

	:detail: Pac-Man plot of Super Average Plot
	:param logical overcast: the condition of data (clear sky/overcast)
	:param character title: the title/description of the plot
	:param double x: the domain of the plot
	:param double y: the range of the plot
	:param character angular: the angular axis label and units
	:param character radial: the radial axis label and units

.. function:: pac.regression(over.bool)

	:detail: Pac-Man residual plot
	:param bool over.bool: the condition of data (clear sky/overcast)

.. function:: chart.histogram(range,xlabel,title)

	:detail: Histograms of defined quantities
	:param double range: the range of the plot
	:param list xlabel: the x-axis label of the plot
	:param list title: the title/description of the histogram

.. function:: poster.plots(over.bool,iter,mean.out)

	:detail: The set of all poster
	:param bool overcast: the condition of data (clear sky/overcast)
	:param list iter: output of iterative.analysis
	:param list mean.out:
	:return: All available poster plots

.. function:: poster1(...)


.. function:: poster2(over.bool,iter,mean.out)

	:detail: The analytics poster plot
	:param logical overcast: the condition of data (clear sky/overcast)
	:param iter: the output of iterative.analysis
	:param mean.out:

.. function:: sensor.chart(...)

	:detail: sensor specific data type distribution charts

.. function:: sensor.time(over.bool)

	:detail: Instrumentation time series plots
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: data.gen(over.bool,dir)

	:detail: creates a datafile containing the date, avg temp, and avg pwv for a defined condition
	:param bool over.bool: the condition of the data (clear sky/overcast)
	:param dir: directory path

.. function:: data.ml(out.dir)

	:detail: creates a datafile containing the machine learning relavant information
	:param character out.dir: directory path

.. function:: data.step(seed,i,coef,r,S)

	:detail: writes a yaml object that contains the analysis results of each step in the iterative analysis
	:param integer seed: the generated seed
	:param integer i:  the step number
	:param list coef: the coefficients of the best-fit
	:param double r: the RSME value
	:param double S: the standard deviation
	:return: a yaml object
	:rtype: list

.. function:: data.final(out.dir,lengths,frac.kept,coef,std,rmse,over.bool=args$overcast)

	:detail: writes the final results of iterative.analysi
	:param character out.dir: the output directory
	:param list lengths: list of lengths
	:param list coef: the average coefficients of the best-fit
	:param double std: the average standard deviation
	:param double rmse: the average rsme values
	:param logical over.bool: the condition of data (clear sky/overcast)

.. function:: visual.products(set,mean.out,datetime=datetime,over.bool=args$overcast)

	:detail: saves plot sets
	:param character set: the --set parameter
	:param mean.out: output of mean.filter
	:param datetime: the datetime range of the data
	:param logical over.bool: the condition of data (clear sky/overcast)

----------
pmat_run.r
----------
	:module: Precipitable-Water Model Analysis Tool
	:synopsis: The main file for PMAT. Documentation available at <https://docs.pmat.app>.
	:module: Precipitable-Water Model Analysis Tool
	:synopsis: The main file for PMAT. Documentation available at <https://docs.pmat.app>.

--------------
pmat_utility.r
--------------
	:module: Precipitable Water Model Analysis Tool: Utility
	:synopsis: general functions for PMAT

.. function:: logg(msglevel,msg,dir=out.dir,lev="INFO")

	:detail: creates log entries for _log.txt
	:param character msglevel: log level of the message
	:param character msg: the message itself
	:param character dir: directory that stores the log file
	:param character lev: the log level of the user

.. function:: aloha.first()

	:detail: shows first time user information

.. function:: aloha.startup()

	:detail: shows title banner for program

.. function:: aloha.closing()

	:detail: cleans up files and ends the program

.. function:: reset_time(datetime)

	:detail: A function that sets the time to 00:00:00
	:param character datetime: a Date or datetime object
	:return: A datetime object with time 00:00:00
	:rtype: double

.. function:: time_axis_init(date)

	:detail: A function that calculates the min, max, and position of the tick marks for
	:param double date: A date or datetime object
	:return: The max, min, and tick mark positions
	:rtype: list

.. function:: time_axis(datetime)

	:detail: A function that sets the x-axis format for time series plots
	:param double datetime: A date or datetime object

.. function:: stnd_title(des,overcast)

	:detail: A function that generates the title based on
	:param character des: the description of the plot
	:param logical overcast: the sky condition
	:return: a title string
	:rtype: character
