
macro(set_compile_option_group OPTION_GROUP)
    # Set OPTION_GROUP to ARGN if user has not overridden
    if(NOT "$ENV{${FLAG_GROUP}}" STREQUAL "")
        set(${OPTION_GROUP} $ENV{${FLAG_GROUP}})
        set(SOURCE "user override")
    else()
        string(REPLACE ";" " " SPACE_DELIMITED "${ARGN}")
        set(${OPTION_GROUP} ${SPACE_DELIMITED})
        set(SOURCE "default")
    endif()
    
    message("         + ${OPTION_GROUP}='${${OPTION_GROUP}}'")
endmacro(set_compile_option_group)


message(STATUS "(F) Compiler flag groups:")

# Options based on compiler family
set_compile_option_group(FC_FAMILY_FLAGS
    -fPIC -cpp -w -auto -noalign -convert big_endian -O2 -vec-report0 
    -fp-model source -openmp -mcmodel=medium -shared-intel -traceback
    -DLINUX_IFORT
)

# Diagnostic flags
set_compile_option_group(DIAG_FLAGS
    # -DNC_DIAG
    -DBPCH_DIAG
    -DBPCH_TIMESER
    -DBPCH_TPBC
)

# Met flags
set_compile_option_group(MET_FLAGS
    -DGEOS_FP
)

# Horizontal grid flags
set_compile_option_group(HGRID_FLAGS
    -DGRID4x5
)

# Miscellaneous flags
set_compile_option_group(MISC_FLAGS
    -DUCX
    -DUSE_REAL8 
    -DUSE_TIMERS
)

message(STATUS "(F) Setting global compiler flags to concatenation of all groups:")
# Group all subgroups into GLOBAL_COMPILER_FLAGS
set_compile_option_group(GLOBAL_COMPILER_FLAGS
    ${FC_FAMILY_FLAGS}
    ${DIAG_FLAGS}
    ${MET_FLAGS}
    ${HGRID_FLAGS}
    ${MISC_FLAGS}
)