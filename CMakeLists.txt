cmake_minimum_required(VERSION 3.0.2)

project(GEOS_Chem VERSION 12.0.0 LANGUAGES Fortran)

# Include helper functions
include(${CMAKE_SOURCE_DIR}/CMakeScripts/gc_targets.cmake)

# Set global compiler options
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")

# NetcdfUtils
add_subdirectory(NcdfUtil)