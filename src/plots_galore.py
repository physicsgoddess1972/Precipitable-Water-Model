import matplotlib
from  matplotlib import pyplot as plt
from numpy import *
from scipy.optimize import curve_fit

fname   = "../data/master_data.csv"
data    = open(fname, 'r')
read    = data.readlines()

def analytical(H,T):
	r = 0.997
	g = 9.806
	p = 856.756
	P1 = (1.6908/(r*g))*(log1p(absolute(float(H)/100)*(6.1121*exp((17.502*T)/(T+240.97)))))
	P2 = (1.6908/(r*g))*(log1p(absolute(p - (float(H)/100)*(6.1121*exp((17.502*T)/(T+240.97))))))
	PW = P1 + P2
	return PW
## Gets Data from CSV file without the Overcast label
def overcast():
	overcast1 	= []
	overcast2 	= []
	overcast3 	= []
	overcast4 	= []
	overcast5 	= []
	c 		= [z.split(',') for z in read]
	del c[0]
	for j in c:
		if j[11] != 'overcast':
			overcast1.append(j[6])
			overcast2.append(j[7])
			overcast3.append(j[8])
			overcast4.append(j[9])
			overcast5.append(j[10])
		else:
			continue
	y1 = overcast1
	y2 = overcast2
	y3 = overcast3
	y4 = overcast4
	y5 = overcast5
	return y1,y2,y3,y4,y5

## Creates an exponential fit
def trendline(x, input, d, e):
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

	def func(x,a,b):
		return a * exp(-b * x)

	popt, pcov  = curve_fit(func, t, avg, p0=(d,e))
	t1          = linspace(min(t), max(t), len(t))
	yy          = func(t1, *popt)

	residuals   = avg - func(t, *popt)
	ss_res      = sum(residuals**2)
	ss_tot      = sum((avg-mean(avg))**2)
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

t, avg, yy, t1,popt,r2, res = trendline(x, super_avg, 19.7, 0.0306)
residual    = array([float(line) for line in res])


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

## Super average of the data (includes exponential fit)
plt.figure(3)
plt.subplots_adjust(bottom=0.16)
plt.scatter(t,avg,color='blueviolet')
plt.plot(t1, yy, color='limegreen',label=r'%2.3f $e^{%2.3f x}$' % tuple(popt))

plt.title("Correlation between Mean Precipitable Water and Temperature")
plt.xlabel("Zenith Sky Temperature [C]")
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

## Residual Plot
plt.figure(4)
plt.subplots_adjust(bottom=0.16)

plt.scatter(t,res,color='royalblue')
plt.title("Residual Plot for the Mean Precipitable Water and Temperature")
plt.xlabel("Zenith Sky Temperature [C]")
plt.ylabel("$\sigma$")

## Analytical Solution
# PW = analytical(25, x) * 10
# t, avg, yy, t1,popt,r2, res = trendline(x, PW, 19.7,0.5)
#
# plt.figure(5)
# plt.subplots_adjust(bottom=0.16)
# plt.scatter(x,PW,color='black')
# plt.plot(t1, yy, color='limegreen',label=r'%2.3f $e^{%2.3f x}$' % tuple(popt))
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