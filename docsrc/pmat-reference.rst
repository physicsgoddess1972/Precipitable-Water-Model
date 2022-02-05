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

.. function:: time_series.plots(datetime,overcast)

	:param date: the datestamp of the data
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: All available time series plots

.. function:: time9()

	:param date: the datestamp of the data
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: time.nth_range(range,title,color,leg.lab,ylab)

	:param date: the datestamp of the data
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: time.composite(range,title,color,ylab)

	:param date: the datestamp of the data
	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: analytical.plots(overcast,iter)

	:param bool overcast: the condition of data (clear sky/overcast)
	:return: All available analytical plots

.. function:: analysis.nth_range(overcast,x,y,title,label,color,leg.lab)

	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: plots4(overcast,iter)

	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: plots5(overcast,iter)

	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: pac.plots(overcast)

	:param bool overcast: the condition of data (clear sky / overcast)
	:return: All available Pac-Man plots

.. function:: pac1(overcast)

	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: pac2(overcast)

	:param bool overcast: the condition of data (clear sky/overcast)
	:return: A sky temperature time series plot

.. function:: charts(...)

	:return: PDF of charts

.. function:: chart1(range,xlabel,title)

	:param range: a data range
	:param xlabel: the xaxis label
	:param title: the title of the histogram

.. function:: poster.plots(overcast,iter)

	:param bool overcast: the condition of data (clear sky/overcast)
	:return: All available poster plots

.. function:: poster1(...)


.. function:: poster2(overcast,iter)

	:param bool overcast: the condition of data (clear sky/overcast)

.. function:: instr(overcast)

	:param bool overcast: the condition of the data (clear sky/overcast)
	:return: Instrumentation time series plots and overcast distribution charts

.. function:: chart(...)


.. function:: time(...)


.. function:: data.products(overcast,dir,i)

	:return: datafiles

.. function:: data1(overcast,dir)

	:param bool overcast: the condition of the data (clear sky/overcast)
	:param dir: directory path

.. function:: data2(dir)

	:param dir: directory path

.. function:: visual.products(set,overcast)

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

.. function:: logg(msglevel,msg)

	:param character msglevel:
	:param character msg:

.. function:: first()


.. function:: startup()


.. function:: closing()


.. function:: save(func,name)

	:param list func: the plotting function that will be saved
	:param character name: the name of the file with the plots
	:return: A pdf of the plot set

.. function:: reset_time(datetime)

	:param character datetime: a Date or datetime object
	:return: A datetime object with time 00:00:00
	:rtype: double

.. function:: time_axis_init(date)

	:param double date: A date or datetime object
	:return: The max, min, and tick mark positions
	:rtype: list

.. function:: time_axis(datetime)

	:param double date: A date or datetime object

.. function:: stnd_title(des,overcast)

	:param character des: the description of the plot
	:param logical overcast: the sky condition
	:return: a title string
	:rtype: character
