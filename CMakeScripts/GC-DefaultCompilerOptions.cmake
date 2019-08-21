if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    target_compile_options(BaseTarget INTERFACE
        -cpp -w -auto -noalign
        $<$<BOOL:"${GCHP}">:-convert native>
        $<$<NOT:$<BOOL:"${GCHP}">>:-convert big_endian>
        -fp-model source -mcmodel=medium
        -shared-intel -traceback -DLINUX_IFORT
    )
    set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -check arg_temp_created -debug all -DDEBUG")
elseif("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    target_compile_options(BaseTarget INTERFACE
        -cpp -w -std=legacy -fautomatic -fno-align-commons 
        $<$<BOOL:"${GCHP}">:-fconvert=native>
        $<$<NOT:$<BOOL:"${GCHP}">>:-fconvert=big-endian>
        -fno-range-check -mcmodel=medium -fbacktrace -g -DLINUX_GFORTRAN
    )
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -funroll-loops")
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -gdwarf-2 -gstrict-dwarf -O0 -Wall -Wextra -Wconversion -Warray-temporaries -fcheck-array-temporaries")
else()
    message(FATAL_ERROR "Unknown Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif()