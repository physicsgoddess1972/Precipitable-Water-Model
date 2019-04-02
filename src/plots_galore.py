import matplotlib
from  matplotlib import pyplot as plt
from numpy import *
from scipy.optimize import curve_fit

fname   = "../data/master_data.csv"
data    = open(fname, 'r')
read    = data.readlines()
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

y1,y2,y3,y4,y5 = overcast()
x           = array([float(line) for line in y1])
y1_abq1     = array([float(line) for line in y2])
y1_abq2     = array([float(line) for line in y3])
y2_epz1     = array([float(line) for line in y4])
y2_epz2     = array([float(line) for line in y5])

y1_abq      = (y1_abq1+y1_abq2)/2
y2_epz      = (y2_epz1+y2_epz2)/2
super_avg   = (y1_abq1+y1_abq2+y2_epz1+y2_epz2)/4

def trendline():
    data = []
    domain=[]

    for j,i in zip(x,super_avg):
        if isnan(j) or isnan(i):
            continue
        else:
            data.append(i)
            domain.append(j)

    t = array(domain)
    avg = array(data)

    def func(x,a,b):
        return a * exp(-b * x)

    popt, pcov  = curve_fit(func, t, avg, p0=(19.7, 0.0306))
    t1          = linspace(min(t), max(t), len(t))
    yy          = func(t1, *popt)

    residuals   = avg - func(t, *popt)
    ss_res      = sum(residuals**2)
    ss_tot      = sum((avg-mean(avg))**2)
    r2          = 1 - (ss_res/ss_tot)
    return t, avg, yy, t1,popt,r2

t, avg, yy, t1,popt,r2 = trendline()

plt.figure(1)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Precipitable Water and Temperature")
plt.scatter(x,y1_abq1,color='crimson', label="ABQ 12Z")
plt.scatter(x,y1_abq2, color='deepskyblue', label="ABQ 00Z")
plt.scatter(x,y2_epz1, color='green', label="EPZ 12Z")
plt.scatter(x,y2_epz2, color='darkviolet', label="EPZ 00Z")
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

plt.figure(2)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Mean Precipitable Water and Temperature")
plt.scatter(x,y1_abq,color='crimson', label="ABQ")
plt.scatter(x,y2_epz, color='deepskyblue', label="EPZ")
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

plt.figure(3)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Mean Precipitable Water and Temperature")
plt.scatter(t,avg,color='crimson')

plt.plot(t1, yy, color='darkviolet',label=r'%2.3f $e^{%2.3f x}$' % tuple(popt))
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

plt.show()