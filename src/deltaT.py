import matplotlib
from  matplotlib import pyplot as plt
from numpy import *

fname   = "../data/master_data.csv"
data    = open(fname, 'r')
labels  = []
read    = data.readlines()

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
y1_gro  = array([float(line) for line in y1])
y1_air  = array([float(line) for line in y2])
y2_air  = array([float(line) for line in y3])
y2_gro  = array([float(line) for line in y4])
y3_air  = array([float(line) for line in y5])
y3_gro 	= array([float(line) for line in y6])

deltay1 = y1_gro - y1_air
deltay2 = y2_gro - y2_air
deltay3 = y3_gro - y3_air

plt.xlabel("Date")
plt.xticks(arange(0,len(x), 7), x, rotation=30, fontsize='small', horizontalalignment='center')
plt.subplots_adjust(bottom=0.16)
plt.title("Change in Temperature between Air and Ground")

plt.scatter(arange(0,len(x)),deltay1,color='crimson', label="16710 TE")
plt.scatter(arange(0,len(x)),deltay2, color='deepskyblue', label="FLIRi3")
plt.scatter(arange(0,len(x)),deltay3, color='green', label="AMES")

plt.ylabel("Temperature [C]")

plt.legend(loc="upper right")

plt.show()
