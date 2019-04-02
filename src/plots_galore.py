import matplotlib
from  matplotlib import pyplot as plt
from numpy import *
from scipy import optimize

fname   = "./master_data_overcast_omit.csv"
data    = open(fname, 'r')
labels  = []
read    = data.readlines()
# Pulls the date from the csv/txt file in the format presented in the file. (Format doesn't matter)
def datefromfile():
	y       = [y.split(',')[0] for y in read]
	content = [y.strip() for y in y]
	labels.append(content[0])
	del content[0]
	x       = content
	return x

# Pulls columned data from file. (n) is the column number starting from 0
def range(n):
	y 		= [y.split(',')[n] for y in read]
	content = [y.strip() for y in y]
	labels.append(content[0])
	del content[0]
	y1      = array([float(y) for y in content])
	return y1

# Output of the function datefromfile()
x        = range(6)
y1_abq1  = range(7)
y1_abq2  = range(8)
y2_epz1  = range(9)
y2_epz2  = range(10)

y1_abq = (y1_abq1+y1_abq2)/2
y2_epz = (y2_epz1+y2_epz2)/2
super_avg = (y1_abq1+y1_abq2+y2_epz1+y2_epz2)/4

#def test_func(x,a,b,c):
#    return a + b * exp(-x/c)

#params, params_covariance = optimize.curve_fit(test_func, x, super_avg, p0=[2,2])

plt.figure(1)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Precipitial Water and Temperature")
plt.scatter(x,y1_abq1,color='crimson', label="ABQ 12Z")
plt.scatter(x,y1_abq2, color='deepskyblue', label="ABQ 00Z")
plt.scatter(x,y2_epz1, color='green', label="EPZ 12Z")
plt.scatter(x,y2_epz2, color='darkviolet', label="EPZ 00Z")
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")


plt.figure(2)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Mean Precipitial Water and Temperature")
plt.scatter(x,y1_abq,color='crimson', label="ABQ")
plt.scatter(x,y2_epz, color='deepskyblue', label="EPZ")
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

plt.figure(3)
idx = isfinite(x) & isfinite(super_avg)
plt.xlabel("Zenith Sky Temperature [C]")
plt.subplots_adjust(bottom=0.16)
plt.title("Correlation between Mean Precipitial Water and Temperature")
plt.scatter(x,super_avg,color='crimson')
plt.ylabel("Precipitable Water [mm]")
plt.legend(loc="upper right")

plt.show()