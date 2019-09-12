from numpy import *

fname1 = "../../data/modtran/modtran1.csv"
fname2 = "../../data/modtran/modtran2.csv"
fname3 = "../../data/modtran/modtran3.csv"
sname = "../../data/modtran/modtran.csv"

fname4 = "../../data/modtran/temp_offset/modtran-5.csv"
#fname5 = "../../data/modtran/temp_offset/modtran-2.csv"
fname5 = "../../data/modtran/temp_offset/modtran0.csv"
#fname7 = "../../data/modtran/temp_offset/modtran+2.csv"
fname6 = "../../data/modtran/temp_offset/modtran+5.csv"
sname1 = "../../data/modtran/temp_offset/modtran.csv"

wl  = loadtxt(fname4, delimiter=",", unpack=True, skiprows=3)[1]
rd1 = loadtxt(fname4, delimiter=",", unpack=True, skiprows=3)[3]
rd2 = loadtxt(fname5, delimiter=",", unpack=True, skiprows=3)[3]
rd3 = loadtxt(fname6, delimiter=",", unpack=True, skiprows=3)[3]
f = open(sname1, "w")
f.write("wavelength, Radiance (T-2), Radiance (T), Radiance (T+2)\n")
for i in range(0, len(wl)):
    f.write(str(wl[i]))
    f.write(",")
    f.write(str(rd1[i]))
    f.write(",")
    f.write(str(rd2[i]))
    f.write(",")
    f.write(str(rd3[i]))
    f.write("\n")
f.close()


