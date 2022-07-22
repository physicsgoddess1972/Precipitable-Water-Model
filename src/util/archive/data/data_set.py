<<<<<<< HEAD
from numpy import *

fname1 = "../../data/modtran/temp_offset_wsv_half/modtran-5.csv"
fname2 = "../../data/modtran/temp_offset_wsv_half/modtran0.csv"
fname3 = "../../data/modtran/temp_offset_wsv_half/modtran+5.csv"
sname = "../../data/modtran/temp_offset_wsv_half/modtran.csv"

# fname1 = "../../data/modtran/ir_band/flir_low.csv"
# fname2 = "../../data/modtran/ir_band/flir_high.csv"
# fname3 = "../../data/modtran/ir_band/ames_low.csv"
# fname4 = "../../data/modtran/ir_band/ames_high.csv"
# sname = "../../data/modtran/ir_band/modtran.csv"

wl  = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[1]
rd1 = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[3]
rd2 = loadtxt(fname2, delimiter=",", unpack=True, skiprows=3)[3]
rd3 = loadtxt(fname3, delimiter=",", unpack=True, skiprows=3)[3]
# rd4 = loadtxt(fname4, delimiter=",", unpack=True, skiprows=3)[3]
# rd5 = loadtxt(fname5, delimiter=",", unpack=True, skiprows=3)[3]

f = open(sname, "w")
# f.write("wavelength,Radiance (WVS 0.25),Radiance (WVS 0.5),Radiance (WVS 1.0),Radiance (WVS 1.5),Radiance (WVS 2.0)\n")
f.write("wavelength, Radiance (T-5), Radiance (T), Radiance (T+5)\n")
for i in range(0, len(wl)):
    f.write(str(wl[i]))
    f.write(",")
    # f.write(str(rd5[i]))
    # f.write(",")
    f.write(str(rd1[i]))
    f.write(",")
    f.write(str(rd2[i]))
    f.write(",")
    f.write(str(rd3[i]))
    # f.write(",")
    # f.write(str(rd4[i]))
    f.write("\n")
f.close()
=======
from numpy import *

fname1 = "../../data/modtran/temp_offset_wsv_half/modtran-5.csv"
fname2 = "../../data/modtran/temp_offset_wsv_half/modtran0.csv"
fname3 = "../../data/modtran/temp_offset_wsv_half/modtran+5.csv"
sname = "../../data/modtran/temp_offset_wsv_half/modtran.csv"

# fname1 = "../../data/modtran/ir_band/flir_low.csv"
# fname2 = "../../data/modtran/ir_band/flir_high.csv"
# fname3 = "../../data/modtran/ir_band/ames_low.csv"
# fname4 = "../../data/modtran/ir_band/ames_high.csv"
# sname = "../../data/modtran/ir_band/modtran.csv"

wl  = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[1]
rd1 = loadtxt(fname1, delimiter=",", unpack=True, skiprows=3)[3]
rd2 = loadtxt(fname2, delimiter=",", unpack=True, skiprows=3)[3]
rd3 = loadtxt(fname3, delimiter=",", unpack=True, skiprows=3)[3]
# rd4 = loadtxt(fname4, delimiter=",", unpack=True, skiprows=3)[3]
# rd5 = loadtxt(fname5, delimiter=",", unpack=True, skiprows=3)[3]

f = open(sname, "w")
# f.write("wavelength,Radiance (WVS 0.25),Radiance (WVS 0.5),Radiance (WVS 1.0),Radiance (WVS 1.5),Radiance (WVS 2.0)\n")
f.write("wavelength, Radiance (T-5), Radiance (T), Radiance (T+5)\n")
for i in range(0, len(wl)):
    f.write(str(wl[i]))
    f.write(",")
    # f.write(str(rd5[i]))
    # f.write(",")
    f.write(str(rd1[i]))
    f.write(",")
    f.write(str(rd2[i]))
    f.write(",")
    f.write(str(rd3[i]))
    # f.write(",")
    # f.write(str(rd4[i]))
    f.write("\n")
f.close()
>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
