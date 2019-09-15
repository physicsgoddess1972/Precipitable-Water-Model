from numpy import *

fname1 = "../../data/modtran/radiance/modtran1.csv"
fname2 = "../../data/modtran/radiance/modtran2.csv"
fname3 = "../../data/modtran/radiance/modtran3.csv"
fname4 = "../../data/modtran/radiance/modtran4.csv"
sname = "../../data/modtran/radiance/modtran.csv"

# fname4 = "../../data/modtran/temp_offset/modtran-5.csv"
# #fname5 = "../../data/modtran/temp_offset/modtran-2.csv"
# fname5 = "../../data/modtran/temp_offset/modtran0.csv"
# #fname7 = "../../data/modtran/temp_offset/modtran+2.csv"
# fname6 = "../../data/modtran/temp_offset/modtran+5.csv"
# sname1 = "../../data/modtran/temp_offset/modtran.csv"

wl  = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[1]
rd1 = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[3]
rd2 = loadtxt(fname2, delimiter=",", unpack=True, skiprows=3)[3]
rd3 = loadtxt(fname3, delimiter=",", unpack=True, skiprows=3)[3]
rd4 = loadtxt(fname4, delimiter=",", unpack=True, skiprows=3)[3]
f = open(sname, "w")
f.write("wavelength, Radiance (WVS 0.5), Radiance (WVS 1.0), Radiance (WVS 1.5), Radiance (WVS 2.0)\n")
for i in range(0, len(wl)):
    f.write(str(wl[i]))
    f.write(",")
    f.write(str(rd3[i]))
    f.write(",")
    f.write(str(rd1[i]))
    f.write(",")
    f.write(str(rd4[i]))
    f.write(",")
    f.write(str(rd2[i]))
    f.write("\n")
f.close()


