import matplotlib
from  matplotlib import pyplot as plt
from numpy import *

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
x       = datefromfile()
y1_gro  = range(1)
y1_air  = range(2)
y2_air  = range(3)
y2_gro  = range(4)
y3_air  = range(5)
y3_gro 	= range(6)

deltay1 = y1_gro - y1_air
deltay2 = y2_gro - y2_air
deltay3 = y3_gro - y3_air

plt.xlabel(labels[0])
plt.xticks(arange(0,len(x), 7), x, rotation=30, fontsize='small', horizontalalignment='center')
plt.subplots_adjust(bottom=0.16)
plt.title("Change in Temperature between Air and Ground")

plt.scatter(arange(0,len(x)),deltay1,color='crimson', label="16710 TE")
plt.scatter(arange(0,len(x)),deltay2, color='deepskyblue', label="FLIRi3")
plt.scatter(arange(0,len(x)),deltay3, color='green', label="AMES")

plt.ylabel("Temperature [C]")

plt.legend(loc="upper right")

plt.show()