**************
PMAT Reference
**************


---------------
pmat_analysis.r
---------------
	:module: Precipitable Water Model Analysis Tool: Analysis
	:synopsis: This module contains analysis functions

.. function:: exp.regression(r,t=NULL,nan.out,data_indx)

	:detail: Function includes all of the stuff to generate the exponential regression model with intervals
	:param list r: output of sky.analysis
	:param double t: training fraction
	:param integer range: range of date indices to be used
	:return: returns the data series and model statistics
	:rtype: list

.. function:: lin.regression(x,y)

	:detail: Linear regression function
	:param double x: the domain of the dataset
	:param double y: the range of the dataset
	:return: returns the data series and model statistics
	:rtype: list

.. function:: data.partition(x,y,train_size=0.7)

	:detail: splits the data into a training/testing set
	:param list x: domain of the data
	:param list y: range of the data
	:param double train_size: fraction of the data in the testing set
	:return: a list containing the training and testing sets
	:rtype: list

.. function:: iterative.analysis(results,dir,obool,nan.out,mean.out)

	:detail: computes regression statistics and outputs to a yaml file
	:param logical overcast: boolean to determine label
	:param string dir: directory file path for _output.yml
	:param logical obool: determine whether to generate new _output.yml
	:param list nan.out: output of nan.filter
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

.. function:: colscheme(range)

	:detail: a function that generates an array of colors based on the number of elements
	:param list range: a list of data series
	:return: a list of colors
	:rtype: list

.. function:: mean.filter(pw,n)

	:detail: filters the data based on the comparison of the daily std and the average std of the dataset
	:param pw: precipitable water data
	:param n: threshold
	:return: an array of indicies for PWV values to be analyzed
	:rtype: list

.. function:: dna.filter(fover)

	:detail: removes data labels as Do Not Analyze
	:param fover: overcast.filter results
	:return: overcast.filter results with DNA points removed
	:rtype: list

.. function:: nan.filter(stuff)

	:detail: removes nan values from a set of lists
	:param list stuff: list of arrays
	:return: returns list with filtered data and the indicies with nans
	:rtype: list

.. function:: inf.counter(bool,snsr_data,label)

	:detail: identifies the -Inf values
	:param logical bool: decides if -Inf is not replaced with NaN
	:param list snsr_data: the dataset
	:param character label: the identifer for the dataset (e.g. sky, gro, skyo, groo)
	:return: data set that replaces all -Infs for NaN (If bool == FALSE).
	:rtype: list

.. function:: index.norm(x)

	:detail: calculates the normalized index of the dataset
	:param double x: data range
	:return: an array of values between 0 and 1
	:rtype: double

.. function:: overcast.filter(col_con,col_date,col_com,pw_name,snsr_name,cloud_bool)

	:detail: Filters our data with overcast condition
	:param integer col_con: column index for condition labels
	:param integer col_date: column index for date stamp
	:param integer col_com: column index for comments
	:param list pw_name: pw measurement labels
	:param list snsr_name: sensor labels
	:return: A list of lists containing either clear-sky/overcast data
	:rtype: list

.. function:: sky.processing(overcast)

	:detail: Computes average values and weighted averages
	:param list overcast: results of the overcast.filter function
	:return: series of arrays including average PWV, RH, etc.
	:rtype: list

---------------
pmat_products.r
---------------
	:module: Precipitable Water Model Analysis Tool: Products
	:synopsis: plotting functions for PMAT

.. function:: time.pwindex(datetime)

	:detail: Normalized PWV index for both clear sky and overcast data
	:param date: the datestamp of the data

.. function:: time.nth_range(range,title,color,leg.lab,ylab,datetime,overcast)

	:detail: Multirange Time Series plot series
	:param date: the datestamp of the data
	:param bool overcast: the condition of data (clear sky/overcast)

.. function:: time.composite(range,title,color,ylab,datetime,overcast)

	:detail: Time Series composite plot series
	:param date: the datestamp of the data
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: analysis.nth_range(overcast,x,y,title,label,color,leg.lab)

	:detail: Super Average Plot with Exponential Fit
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: analysis.regression(overcast,x,y,des,label,iter,results)

	:detail: Super Average Plot with Exponential Fit
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: analysis.svm(model)


.. function:: pac.compare(overcast,des,x,y,angular,radial)

	:detail: Pac-Man plot of Super Average Plot
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: pac.regression(overcast)

	:detail: Pac-Man residual plot
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: charts(...)

	:detail: A collection of histograms and charts
	:return: PDF of charts

.. function:: chart1(range,xlabel,title)

	:detail: Histograms of defined quantities
	:param range: a data range
	:param xlabel: the xaxis label
	:param title: the title of the histogram

.. function:: poster.plots(overcast,iter,nan.out,mean.out)

	:detail: The set of all poster
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: All available poster plots

.. function:: poster1(...)


.. function:: poster2(overcast,iter,nan.out,mean.out)

	:detail: The analytics poster plot
	:param bool overcast: the condition of data (clear sky/overcast)

.. function:: sensor.chart(...)

	:detail: overcast distribution charts

.. function:: sensor.time(overcast)

	:detail: Instrumentation time series plots

.. function:: data.gen(overcast,dir)

	:detail: creates a datafile containing the date, avg temp, and avg pwv for a defined condition
	:param bool overcast: the condition of the data (clear sky/overcast)
	:param dir: directory path

.. function:: data.ml(dir)

	:detail: creates a datafile containing the machine learning relavant information
	:param dir: directory path

.. function:: data.step(seed,i,coef,r,S)


.. function:: data.final(dir,clear.len,over.len,train.len,nan.len,frac.kept,coef,std,rmse)


.. function:: visual.products(set,nan.out,mean.out,datetime=datetime,overcast=args$overcast)

	:detail: saves plot sets
	:param character set: the set identifier
	:param logical overcast: ovecast boolean

----------
pmat_run.r
----------
	:module: Precipitable-Water Model Analysis Tool
	:synopsis: The main file for PMAT. Documentation available at <https://docs.pmat.app>.

--------------
pmat_utility.r
--------------
	:module: Precipitable Water Model Analysis Tool: Utility
	:synopsis: general functions for PMAT

.. function:: logg(msglevel,msg,dir=out.dir)

	:detail: creates log entries for _log.txt
	:param character msglevel:
	:param character msg:

.. function:: first()


.. function:: startup()

	:detail: shows title banner for program

.. function:: closing()

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
	:param double date: A date or datetime object

.. function:: stnd_title(des,overcast)

	:detail: A function that generates the title based on
	:param character des: the description of the plot
	:param logical overcast: the sky condition
	:return: a title string
	:rtype: character
