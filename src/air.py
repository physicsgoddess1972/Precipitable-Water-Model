import matplotlib
from  matplotlib import pyplot as plt
from numpy import *

fname   = "../data/master_data.csv"
data    = open(fname, 'r')
read    = data.readlines()
# Pulls the air temperature that are not labeled as overcast
def overcast():
    domain		= []
    overcast1 	= []
    overcast2 	= []
    overcast3 	= []
    c 		= [z.split(',') for z in read]
    del c[0]
    for j in c:
        if j[11] != 'overcast':
            domain.append(j[0])
            overcast1.append(j[2])
            overcast2.append(j[4])
            overcast3.append(j[6])
        else:
            continue
    y1 = overcast1
    y2 = overcast2
    y3 = overcast3
    return domain, y1,y2,y3

x,y1,y2,y3 	= overcast()
range1 = [float(line) for line in y1]
range2 = [float(line) for line in y2]
range3 = [float(line) for line in y3]

plt.xlabel("Date")
plt.xticks(arange(0,len(x), 7), x, rotation=30, fontsize='small', horizontalalignment='center')
plt.subplots_adjust(bottom=0.16)
plt.title("Air Temperature")

plt.scatter(arange(0,len(x)),array(range1),color='crimson', label="1610 TE")
plt.scatter(arange(0,len(x)),array(range2),color='deepskyblue',label="FLIRi3")
plt.scatter(arange(0,len(x)),array(range3), color='green',label="AMES")
plt.ylabel("Temperature [C]")
plt.legend(loc='upper left')
plt.show()
