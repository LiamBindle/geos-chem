cmake_minimum_required(VERSION 3.0.2)

project(GEOS_Chem VERSION 12.0.0 LANGUAGES Fortran)

# Include helper functions
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMakeScripts)
include(${CMAKE_SOURCE_DIR}/CMakeScripts/gc_targets.cmake)

# Set global compiler options
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")

# Find NetCDF
find_package(NetCDF REQUIRED COMPONENTS F90)

add_definitions(
    -DMET=geosfp
    -DGRID=4x5
    -DNEST=n
    -DUCX=y
    -DCHEM=Standard
    -DRRTMG=n
    -DTOMAS12=n
    -DTOMAS15=n
    -DTOMAS30=n
    -DTOMAS40=n
    -DMASSCONS=n
    -DOMP=yes
    -DTIMERS=1
    -DBPCH_DIAG=y
    -DBPCH_RST_IN=n
    -DBPCH_RST_OUT=n

    -cpp -w -auto -noalign -convert big_endian -O2 -vec-report0 -fp-model source -openmp -mcmodel=medium -shared-intel -traceback -DLINUX_IFORT -DBPCH_DIAG -DUCX -DGEOS_FP -DGRID4x5 -DUSE_REAL8 -DUSE_TIMERS
)

# NetcdfUtils
add_subdirectory(KPP/Standard)
add_subdirectory(Headers)
add_subdirectory(GeosUtil)
add_subdirectory(NcdfUtil)
add_subdirectory(GeosUtil2)
add_subdirectory(History)
add_subdirectory(HEMCO)