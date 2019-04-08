import matplotlib
from  matplotlib import pyplot as plt
from numpy import *

fname   = "../data/master_data.csv"
data    = open(fname, 'r')
read    = data.readlines()

# Pulls the date from the csv/txt file in the format presented in the file. (Format doesn't matter)
def datefromfile():
	y       = [y.split(',')[0] for y in read]
	content = [y.strip() for y in y]
	del content[0]
	x       = content
	return x

# Pulls columned data from file. (n) is the column number starting from 0
def range(n):
	y 		= [y.split(',')[n] for y in read]
	content = [y.strip() for y in y]
	del content[0]
	y1      = array([float(y) for y in content])
	return y1

def overcast():
	domain		= []
	overcast1 	= []
	overcast2 	= []
	overcast3 	= []
	overcast4 	= []
	overcast5 	= []
	overcast6 	= []
	c 		= [z.split(',') for z in read]
	del c[0]
	for j in c:
		if j[11] != 'overcast':
			domain.append(j[0])
			overcast1.append(j[1])
			overcast2.append(j[2])
			overcast3.append(j[3])
			overcast4.append(j[4])
			overcast5.append(j[5])
			overcast6.append(j[6])
		else:
			continue
	y1 = overcast1
	y2 = overcast2
	y3 = overcast3
	y4 = overcast4
	y5 = overcast5
	y6 = overcast6
	return domain, y1,y2,y3,y4,y5,y6

x,y1,y2,y3,y4,y5,y6 = overcast()
## TE Thermometer
y1_gro  = array([float(line) for line in y1])
y1_air  = array([float(line) for line in y2])
## FLIRi3 Thermometer
y2_gro  = array([float(line) for line in y3])
y2_air  = array([float(line) for line in y4])
## AMES Thermometer
y3_gro  = array([float(line) for line in y5])
y3_air 	= array([float(line) for line in y6])

# Difference between ground and air temperatures
deltay1 = y1_gro - y1_air
deltay2 = y2_gro - y2_air
deltay3 = y3_gro - y3_air

## Used for Ground Temperature Plots
d1      = datefromfile()
y1      = range(1)
y2      = range(3)
y3 	    = range(5)

## Change in Temperature
plt.figure(1)
plt.subplots_adjust(bottom=0.16)
plt.xticks(arange(0,len(x), 7), [x[i] for i in arange(0, len(x), 7)], rotation=30, fontsize='small', horizontalalignment='center')
plt.scatter(arange(0,len(x)),deltay1,color='crimson', label="16710 TE")
plt.scatter(arange(0,len(x)),deltay2, color='deepskyblue', label="FLIRi3")
plt.scatter(arange(0,len(x)),deltay3, color='green', label="AMES")

plt.title("Change in Temperature between Air and Ground")
plt.xlabel("Date")
plt.ylabel("Temperature [C]")
plt.legend(loc="lower right")


## Air Temperature
plt.figure(2)
plt.subplots_adjust(bottom=0.16)

plt.scatter(arange(0,len(x)),array(y1_air),color='crimson', label="1610 TE")
plt.scatter(arange(0,len(x)),array(y2_air),color='deepskyblue',label="FLIRi3")
plt.scatter(arange(0,len(x)),array(y3_air), color='green',label="AMES")

plt.title("Air Temperature")
plt.xlabel("Date")
plt.ylabel("Temperature [C]")
plt.xticks(arange(0,len(x), 7), [x[i] for i in arange(0, len(x), 7)], rotation=30, fontsize='small', horizontalalignment='center')
plt.legend(loc='upper left')


## Ground Temperature
plt.figure(3)
plt.subplots_adjust(bottom=0.16)
plt.scatter(arange(0,len(d1)),y1,color='crimson', label="1610 TE")
plt.scatter(arange(0,len(d1)),y2, color='deepskyblue', label="FLIRi3")
plt.scatter(arange(0,len(d1)),y3, color='green', label="AMES")

plt.title("Ground Temperature")
plt.xlabel("Date")
plt.ylabel("Temperature [C]")
plt.xticks(arange(0,len(d1), 9), [d1[i] for i in arange(0, len(d1), 9)], rotation=30, fontsize='small', horizontalalignment='center')
plt.legend(loc='upper left')

plt.show()
