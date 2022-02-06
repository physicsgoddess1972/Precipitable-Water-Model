**************
PMAT Reference
**************


---------------
pmat_analysis.r
---------------
	:module: Precipitable Water Model Analysis Tool: Analysis
	:synopsis: This module contains analysis functions

.. function:: exp.regression(results,t,range=c(1:length(results$date)))

	:detail: Function includes all of the stuff to generate the exponential regression model with intervals
	:param list results: output of sky.analysis
	:param double t: training fraction
	:param integer range: range of date indices to be used
	:return: returns the data series and model statistics
	:rtype: list

.. function:: index.norm(x)

	:detail: calculates the normalized index of the dataset
	:param double x: data range
	:return: an array of values between 0 and 1
	:rtype: double

.. function:: inf.counter(bool,snsr_data,label)

	:detail: identifies the -Inf values
	:param logical bool: decides if -Inf is not replaced with NaN
	:param list snsr_data: the dataset
	:param character label: the identifer for the dataset (e.g. sky, gro, skyo, groo)
	:return: data set that replaces all -Infs for NaN (If bool == FALSE).
	:rtype: list

.. function:: iterative.analysis(overcast,dir,obool)

	:detail: computes regression statistics and outputs to a yaml file
	:param logical overcast: boolean to determine label
	:param string dir: directory file path for _output.yml
	:param logical obool: determine whether to generate new _output.yml
	:return: iterative stats and _output.yml
	:rtype: list
	:todo: make the output file pass through a data.products function

.. function:: lin.regression(x,y)

	:detail: Linear regression function
	:param double x: the domain of the dataset
	:param double y: the range of the dataset
	:return: returns the data series and model statistics
	:rtype: list

.. function:: sky.analysis(overcast)

	:detail: Computes average values and weighted averages
	:param list overcast: results of the overcast.filter function
	:return: series of arrays including average PWV, RH, etc.
	:rtype: list

-----------------
pmat_processing.r
-----------------
	:module: Precipitable Water Model Analysis Tool: Pre-processing
	:synopsis: functions for preprocessing

.. function:: colscheme(range)


.. function:: mean.filter(pw,n)


.. function:: data.partition(x,y,train_size=0.7)


.. function:: dna.filter(date,comments,snsr_sky,snsr_gro)


.. function:: overcast.filter(col_con,col_date,col_com,pw_name,snsr_name,cloud_bool)


---------------
pmat_products.r
---------------
	:module: Precipitable Water Model Analysis Tool: Products
	:synopsis: plotting functions for PMAT

.. function:: time9(datetime)

	:detail: Sky Temperature - RH Time Series
	:param date: the datestamp of the data
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

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

.. function:: poster.plots(overcast,iter)

	:detail: The set of all poster
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: All available poster plots

.. function:: poster1(...)


.. function:: poster2(overcast,iter)

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

.. function:: visual.products(set,datetime=datetime,overcast=args$overcast)

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

.. function:: logg(msglevel,msg,dir=args$dir)

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
