#!/usr/bin/env python

import sys
import mpl_helper
import scipy.io

(figure, axes) = mpl_helper.make_fig(top_margin=0.6, right_margin=0.8)

ncf = scipy.io.netcdf_file("../MusicBox_output.nc")
time = ncf.variables["time"].data.copy()

out_filename = "../plot"
for i, arg in enumerate(sys.argv):
  if i == 0:
    continue
  var   = ncf.variables[arg].data.copy()
  units = ncf.variables[arg].units
  axes.plot(time, var, "-", label=arg.replace("_"," ") )
  out_filename += "_" + arg
out_filename += ".pdf"

axes.set_xlabel(r"time / h")
axes.set_ylabel(units.decode('utf-8'))
axes.legend()
axes.grid(True)

print("Writing %s" % out_filename)
figure.savefig(out_filename)
