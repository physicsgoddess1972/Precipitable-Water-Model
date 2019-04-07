import matplotlib
from  matplotlib import pyplot as plt
from numpy import *

x = arange(-60,0, 0.001)
y1 = 8*exp(-(x+35))
y2 = 4*exp(-(x+5))

fig, ax = plt.subplots()

plt.subplots_adjust(bottom=0.16)
plt.title("Mixing Ratio Optical Path")

plt.scatter(x, y2, color="deepskyblue", s=2)
plt.scatter(x,y1,color='crimson', s=2)
plt.scatter(-40, 1187.305, s=30, label=r'$\tau=1$')
plt.scatter(-10, 593.653, s=30, label=r'$\tau=1$')
plt.text(-38, 1187.305, r'$\tau=1$')
plt.text(-8, 593.653, r'$\tau=1$')
plt.xlim(-60,0)
plt.ylim(0, 1600)
plt.ylabel("Altitude [m]")
plt.xlabel("Temperature [C]")
plt.show()