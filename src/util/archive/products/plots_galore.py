import matplotlib
from  matplotlib import pyplot as plt
from numpy import *
from scipy.optimize import curve_fit
from scipy.integrate import quad
import numpy.polynomial.polynomial as poly

fname   = "../data/master_data.csv"
data    = open(fname, 'r')
read    = data.readlines()

# def analytical(H,T, x):
# 	def integrand(T,p):
# 		a = 6.1121
# 		b = 17.502
# 		c = 240.97
# 		return 1/(r*g) * (0.622 * (a * exp((b*T)/(T+c))))/(p-(a * exp((b*T)/(T+c))))
# 	r = 0.997
# 	g = 9.806
# 	p = 856.756
# 	output = []
# 	domain = []
# 	for i in H:
# 		if isnan(i):
# 			continue
# 		else:
# 			PW 	= quad(integrand, 856.756, 0, args=(p))
# 			output.append(PW)
# 			domain.append(x)
# 	return output, domain
## Gets Data from CSV file without the Overcast label
def overcast():
	overcast1 	= []
	overcast2 	= []
	overcast3 	= []
	overcast4 	= []
	overcast5 	= []
	#overcast6 	= []
	c 		= [z.split(',') for z in read]
	del c[0]
	for j in c:
		if j[11] != 'overcast':
			overcast1.append(j[6])
			overcast2.append(j[7])
			overcast3.append(j[8])
			overcast4.append(j[9])
			overcast5.append(j[10])
	#		overcast6.append(j[15])
		else:
			continue
	y1 = overcast1
	y2 = overcast2
	y3 = overcast3
	y4 = overcast4
	y5 = overcast5
	#y6 = overcast6
	return y1,y2,y3,y4,y5#,y6

## Creates an exponential fit
def trendline(x, input, n):
	data = []
	domain=[]

	for j,i in zip(x,input):
		if isnan(j) or isnan(i):
			continue
		else:
			data.append(i)
			domain.append(j)

	t = array(domain)
	avg = array(data)

	if n == "Basic Exp":
		def func(x,a,b):
			return a * exp(b * x)
	elif n == "Ln":
		def func(x,a,b):
			return log(a) + (b*x)
	elif n == "Ln(1+x)":
		def func(x,a,b):
			return log1p(a*exp(b*x))
	elif n == "Linear":
		def func(x,a,b):
			return a*x + b
	else:
		print("No fit selected")

	t1          = linspace(min(t), max(t), len(t))
	popt, pcov  = curve_fit(func, t, avg)
	yy          = func(t1, *popt)

	residuals 	= avg - yy
	ss_res 	    = sum(residuals ** 2)
	ss_tot 	    = sum((avg - mean(avg)) ** 2)
	r2          = 1 - (ss_res/ss_tot)
	return t, avg, yy, t1,popt,r2, residuals

y1,y2,y3,y4,y5 = overcast()

x           = array([float(line) for line in y1])
y1_abq1     = array([float(line) for line in y2])
y1_abq2     = array([float(line) for line in y3])
y2_epz1     = array([float(line) for line in y4])
y2_epz2     = array([float(line) for line in y5])

y1_abq      = (y1_abq1+y1_abq2)/2
y2_epz      = (y2_epz1+y2_epz2)/2
super_avg   = (y1_abq1+y1_abq2+y2_epz1+y2_epz2)/4
#rel_humid 	= array([float(line) for line in y6])

# Logged
t_log1, avg_log1, yy_log1, t1_log1, popt_log1, r2_log1, res_log1    = trendline(x, log1p(super_avg), "Ln(1+x)")
t_log2, avg_log2, yy_log2, t1_log2, popt_log2, r2_log2, res_log2    = trendline(x, log(super_avg), "Ln")

# Not logged
t, avg, yy, t1, popt, r2, res = trendline(x, super_avg, "Basic Exp")

## Individual Data
plt.figure(1)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Precipitable Water and Temperature")
plt.scatter(x,y1_abq1,  color='crimson',     label="ABQ 12Z")
plt.scatter(x,y1_abq2,  color='deepskyblue', label="ABQ 00Z")
plt.scatter(x,y2_epz1,  color='green',       label="EPZ 12Z")
plt.scatter(x,y2_epz2,  color='darkviolet',  label="EPZ 00Z")
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

## Location-based Averages
plt.figure(2)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Location-Based Mean Precipitable Water\n and Temperature")
plt.scatter(x,y1_abq,   color='orange',     label="ABQ")
plt.scatter(x,y2_epz,   color='dodgerblue', label="EPZ")
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

#PW, domain = zip(analytical(rel_humid, x, x))
#print(len(PW), len(domain))
## Super average of the data (includes exponential fit)
plt.figure(3)
plt.subplots_adjust(bottom=0.16)
plt.scatter(t,log1p(avg),color='blueviolet')
#plt.plot(t1_log1, yy_log1, color='limegreen',label='ln(%2.3f)$ + %2.3f x$\t|\tln(x + 1)' % tuple(popt_log1))
plt.plot(t1_log2, yy_log2, color='mediumblue',label='ln(%2.3f)$ + %2.3f x$\t|\tln(x)' % tuple(popt_log2))
#plt.plot(t1, log(25.08) + 0.041*t1, color="crimson")
plt.title("Correlation between Logged Mean Precipitable Water and Temperature")
plt.xlabel("Zenith Sky Temperature [C]")
plt.ylabel("ln(PW) [mm]")
plt.legend(loc="lower right")

plt.figure(4)
plt.subplots_adjust(bottom=0.16)
plt.scatter(t,avg,color='blueviolet')
plt.plot(t1, yy, color='limegreen',label=r'%2.3f$e^{%2.3f x}$' % tuple(popt))
plt.plot(t1, 25.08 * exp(0.041*t1), color="crimson")
plt.title("Correlation between Mean Precipitable Water and Temperature")
plt.xlabel("Zenith Sky Temperature [C]")
plt.ylabel("PW [mm]")
plt.legend(loc="lower right")
#
## Residual Plot
plt.figure(5)
plt.subplots_adjust(bottom=0.16)
plt.scatter(t_log1,res_log1,color='limegreen')
#plt.scatter(t_log2,res_log2,color='mediumblue')
plt.title("Residual Plot for the Logged Mean Precipitable Water and Temperature")
plt.xlabel("Zenith Sky Temperature [C]")
plt.ylabel("$\sigma$")

plt.figure(6)
plt.subplots_adjust(bottom=0.16)
plt.scatter(t,res,color='mediumblue')
plt.title("Residual Plot for the Mean Precipitable Water and Temperature")
plt.xlabel("Zenith Sky Temperature [C]")
plt.ylabel("$\sigma$")
#

## Analytical Solution
#t, avg, yy, t1,popt,r2, res = trendline(x, PW, 19.7,0.5)

# plt.figure(5)
# plt.subplots_adjust(bottom=0.16)
# #plt.plot(t1, yy, color='limegreen',label=r'%2.3f $e^{%2.3f x}$' % tuple(popt))
# plt.title("Analytical Precipitable Water (Work in Progress)")
# plt.xlabel("Zenith Sky Temperature [C]")
# plt.ylabel("Precipitable Water [mm]")
# plt.legend(loc='best')

## Residual Plot for Analytical
# plt.figure(6)
# plt.subplots_adjust(bottom=0.16)
#
# plt.scatter(t,res,color='royalblue')
# plt.title("Residual Plot for the Mean Precipitable Water and Temperature")
# plt.xlabel("Zenith Sky Temperature [C]")
# plt.ylabel("$\sigma$")
plt.show()