cmake_minimum_required(VERSION 3.0.2)

project(GEOS_Chem VERSION 12.0.0 LANGUAGES Fortran)


# Get our compiler options
include(${CMAKE_SOURCE_DIR}/CMakeScripts/gc_util.cmake)
include(${CMAKE_SOURCE_DIR}/CMakeScripts/gc_compiler_options.cmake)

set(CMAKE_Fortran_FLAGS ${FC_OPTIONS})

# Include helper functions
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMakeScripts)
include(${CMAKE_SOURCE_DIR}/CMakeScripts/gc_targets.cmake)

# Find NetCDF
find_package(NetCDF REQUIRED COMPONENTS F90)

# Include targets from subdirectories
add_subdirectory(KPP/Standard)
add_subdirectory(Headers)
add_subdirectory(GeosUtil)
add_subdirectory(NcdfUtil)
add_subdirectory(GeosUtil2)
add_subdirectory(History)
add_subdirectory(HEMCO)
add_subdirectory(ISOROPIA)
if(${RRTMG})
    add_subdirectory(GeosRad)
endif()
add_subdirectory(GeosCore)
