import matplotlib
from  matplotlib import pyplot as plt
from numpy import *


fname   = "./data_air.csv"
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
x   = datefromfile()
y1  = range(2)
y2  = range(3)
y3 	= range(4)
def dualy():
	fig,ax1 = plt.subplots()
	#plt.xlabel(labels[0])
	plt.xticks(arange(0,len(x)), x, rotation=30, fontsize='small', horizontalalignment='center')
	#plt.subplots_adjust(bottom=0.16)
	plt.title("Testing plot:\nIf you can see this then it may or may not have worked")

	# Example plot
	ax1.scatter(arange(0,len(x)),y1,color='crimson', label=labels[1])
	ax1.set_ylabel("This thing")

	ax2 = ax1.twinx()
	ax2.scatter(arange(0,len(x)),y2,color='gold', label=labels[2])
	ax2.set_ylabel("This other thing")
	#fig.legend(loc=1, bbox_to_anchor=(1,1), bbox_transform=ax1.transAxes)

def singley():
	plt.xlabel(labels[0])
	plt.xticks(arange(0,len(x)), x, rotation=30, fontsize='small', horizontalalignment='center')
	plt.subplots_adjust(bottom=0.16)
	plt.title("Testing plot:\nIf you can see this then it may or may not have worked")
	# Example plot
	plt.scatter(arange(0,len(x)),y1,color='crimson', label=labels[1])
	plt.scatter(arange(0,len(x)),y2, color='deepskyblue', label=labels[2])
	plt.scatter(arange(0,len(x)),y3, color='green', label=labels[3])

	plt.ylabel("This thing")

singley()
#dualy()
plt.legend()
plt.show()