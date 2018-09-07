

## CMake Crash Course
CMake is a utility for _generating_ build systems. This means that CMake generates the Makefiles that build GEOS-Chem. Usage of the `cmake` command looks like the following.
```bash
    cmake [ <argument> ... ] <path to source>
```  

### Creating the build system

Start by cloning GEOS-Chem into `~/geos-chem`

```bash
~ $ git clone --recursive https://github.com/LiamBindle/geos-chem.git GEOS-Chem
~ $ mkdir build 
~ $ mkdir rundir
~ $ cd build
```

Note that this repo has submodules, so the `--recursive` flag will clone those as well (this tutorial needs the `RunDirSetup/UT` submodule).

For this tutorial, `<path to source>` is `../GEOS-Chem` (path to the GEOS-Chem repo). Note that when you call `cmake`, the argument list must come _before_ `<path to source>` (this will be useful later).

Next, run `cmake` to see the default build system configuration.
```bash
~/build $ cmake ../GEOS-Chem
```

Next we will set the `RUNDIR` and `EXTDATA` _cache entries_ since their default values are placeholders.

In `build/` you will see `CMakeCache.txt` which is used by CMake to retain _cache entries_ between `cmake` commands (e.g. `MET`, `DIAG`, `GC_DEFINES`, `RUNDIR`, etc.). Cache entries are my with the `-D<name>=<value>` argument.


```bash
~/build $ cmake -DRUNDIR=~/rundir -DEXTDATA=/data10/ctm ../GEOS-Chem
```

Replacing `/data10/ctm` with the path to your ExtData. At this point, the output of `cmake` should look like the following.

```bash
~/build $ cmake ../GEOS-Chem
-- General settings:
  * MET:          [GEOS_FP]  MERRA2
  * NESTED:       TRUE  [FALSE]
  * GRID:         [4x5]  2x2.5
  * MECH:         [Standard]  Tropchem  SOA_SVPOA
  * RRTMG:        TRUE  [FALSE]

-- Additional definitions:
  + DIAG:         BPCH_DIAG  BPCH_TIMESER  BPCH_TPBC
  + EXTRA:        UCX  USE_REAL8  USE_TIMERS

-- Resulting definitions/options:
  + GC_DEFINES:   GEOS_FP  GRID4x5  BPCH_DIAG  BPCH_TIMESER  BPCH_TPBC  UCX
  ...             USE_REAL8  USE_TIMERS
  + FC_OPTIONS:   -fPIC  -cpp  -w  -auto  -noalign  -convert big_endian
  ...             -O2  -vec-report0  -fp-model source  -openmp
  ...             -mcmodel=medium  -shared-intel  -traceback  -DLINUX_IFORT

-- Build configuration hash: f549694

-- Run directory setup:
  + START:        2016-07-01T00
  + END:          2016-08-01T00
  + RUNDIR:       ~/rundir
  + EXTDATA:      /data10/ctm

-- Static library target: libkpp_firstpass.a (s:3/d:0)
-- Static library target: libKpp.a (s:14/d:1)
-- Static library target: libHeaders.a (s:18/d:1)
-- Static library target: libGeosUtil.a (s:3/d:1)
-- Static library target: libnc.a (s:12/d:2)
-- Static library target: libGeosUtil2.a (s:13/d:1)
-- Static library target: libHistory.a (s:7/d:1)
-- Static library target: libHCO.a (s:35/d:1)
-- Static library target: libHCOX.a (s:24/d:1)
-- Static library target: libHCOI.a (s:2/d:1)
-- Static library target: libIsoropia.a (s:1/d:0)
-- Static library target: libTransport.a (s:8/d:2)
-- Static library target: libGeosCore.a (s:82/d:1)
-- Executable target: geos (s:1/d:1)
-- Run directory setup is enabled
-- Configuring done
-- Generating done
-- Build files have been written to: /stetson-home/bindle/build
```

The Make build system now exists is `~/build/`.

### Building GEOS-Chem
To build GEOS-Chem run `make` (note that `-j4` will build 4 files simultaneously). Note that restarting `make` is perfeclty safe.
```bash
~/build $ make -j4 install
Running gcCopyRunDirs
[  2%] Building Fortran object ISOROPIA/CMakeFiles/Isoropia.dir/isorropiaII_main_mod.F.o
[  2%] Building Fortran object KPP/Standard/CMakeFiles/kpp_firstpass.dir/gckpp_Precision.F90.o
[  2%] Built target rundir
[  3%] Building Fortran object KPP/Standard/CMakeFiles/kpp_firstpass.dir/gckpp_Parameters.F90.o
[  3%] Building Fortran object KPP/Standard/CMakeFiles/kpp_firstpass.dir/gckpp_Monitor.F90.o
[  3%] Linking Fortran static library libkpp_firstpass.a
[  3%] Built target kpp_firstpass
[  4%] Building Fortran object Headers/CMakeFiles/Headers.dir/charpak_mod.F90.o
[  4%] Building Fortran object Headers/CMakeFiles/Headers.dir/precision_mod.F.o
[  4%] Building Fortran object Headers/CMakeFiles/Headers.dir/inquireMod.F90.o
[  4%] Building Fortran object Headers/CMakeFiles/Headers.dir/registry_params_mod.F90.o
[  5%] Building Fortran object Headers/CMakeFiles/Headers.dir/physconstants.F.o
[  5%] Building Fortran object Headers/CMakeFiles/Headers.dir/errcode_mod.F90.o
                                        .
                                        .
                                        .
[ 98%] Building Fortran object GeosCore/CMakeFiles/GeosCore.dir/set_prof_o3.F.o
[ 99%] Building Fortran object GeosCore/CMakeFiles/GeosCore.dir/YuIMN_Code.F.o
[ 99%] Linking Fortran static library libGeosCore.a
[ 99%] Built target GeosCore
[100%] Building Fortran object GeosCore/CMakeFiles/geos.dir/main.F.o
[100%] Linking Fortran executable geos
[100%] Built target geos
Install the project...
-- Install configuration: ""
-- Installing: /stetson-home/bindle/rundir/geos
-- Set runtime path of "/stetson-home/bindle/rundir/geos" to ""
-- Up-to-date: /stetson-home/bindle/rundir
-- Installing: /stetson-home/bindle/rundir/FJX_spec.dat
-- Installing: /stetson-home/bindle/rundir/input.geos
                                        .
                                        .
                                        .
-- Installing: /stetson-home/bindle/rundir/h2so4.dat
-- Installing: /stetson-home/bindle/rundir/HEMCO_Diagn.rc
```

### Running GEOS-Chem
To run GEOS-Chem, navigate to your run directory (where you installed to) and execute `geos`.
```bash
~/build $ cd ~/rundir
~/rundir $ ./geos
```

