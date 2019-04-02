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

x   = datefromfile()
y1  = range(1)
y2  = range(3)
y3 	= range(5)


plt.xlabel("Date")
plt.xticks(arange(0,len(x), 7), x, rotation=30, fontsize='small', horizontalalignment='center')
plt.subplots_adjust(bottom=0.16)
plt.title("Ground Temperature")
# Example plot
plt.scatter(arange(0,len(x)),y1,color='crimson', label="1610 TE")
plt.scatter(arange(0,len(x)),y2, color='deepskyblue', label="FLIRi3")
plt.scatter(arange(0,len(x)),y3, color='green', label="AMES")

plt.ylabel("Temperature [C]")

plt.legend(loc='upper left')
plt.show()