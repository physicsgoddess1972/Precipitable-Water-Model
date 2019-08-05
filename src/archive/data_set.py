from numpy import *

fname1 = "../../data/modtran1.csv"
fname2 = "../../data/modtran2.csv"
sname = "../../data/modtran.csv"
wl  = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[1]
rd1 = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[3]
rd2 = loadtxt(fname2, delimiter=",", unpack=True, skiprows=3)[3]

f = open(sname, "w")
f.write("wavelength, Radiance (1), Radiance (2)\n")
for i in range(0, len(wl)):
    f.write(str(wl[i]))
    f.write(",")
    f.write(str(rd1[i]))
    f.write(",")
    f.write(str(rd2[i]))
    f.write("\n")

f.close()


