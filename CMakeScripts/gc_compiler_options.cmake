
function(addFlagGroup TARGET GENERATED_FLAGS)
    add_library(${TARGET} INTERFACE)
    message(STATUS "(F) Compiler option group: ${TARGET}")

    if(NOT "$ENV{${TARGET}}" STREQUAL "")
        # User-override
        target_compile_options(${TARGET} INTERFACE
            $ENV{${TARGET}}
        )
        message("       * OVERRIDDEN with the environment variable \"${TARGET}\"")
    else()
        target_compile_options(${TARGET} INTERFACE
            ${GENERATED_FLAGS}
        )
        message("       To override, set the environment variable \"${TARGET}\"")
    endif()
    get_target_property(flags ${TARGET} INTERFACE_COMPILE_OPTIONS)
    string(REPLACE ";" " " flags "${flags}")
    message("       Flags: ${flags}")
endfunction(addFlagGroup)


set(COMPILER_FAM_FLAGS 
    -fPIC -cpp -w -auto -noalign -convert big_endian -O2 -vec-report0 
    -fp-model source -openmp -mcmodel=medium -shared-intel -traceback

    -DLINUX_IFORT
)
addFlagGroup(CompilerFamilyFlags "${COMPILER_FAM_FLAGS}")


set(DIAG_FLAGS
    # -DNC_DIAG
    -DBPCH_DIAG
    -DBPCH_TIMESER
    -DBPCH_TPBC

)
addFlagGroup(DiagFlags "${DIAG_FLAGS}")


set(MET_FLAGS
    -DGEOS_FP
)
addFlagGroup(MetFlags "${MET_FLAGS}")

set(HGRID_FLAGS
    -DGRID4x5
)
addFlagGroup(HGridFlags "${HGRID_FLAGS}")

set(MISC_FLAGS
    -DUCX
    -DUSE_REAL8 
    -DUSE_TIMERS
)
addFlagGroup(MiscFlags "${MISC_FLAGS}")

# Get global flags
get_target_property(flags CompilerFamilyFlags INTERFACE_COMPILE_OPTIONS)
set(GLOBAL_FLAGS "${flags}")
get_target_property(flags DiagFlags INTERFACE_COMPILE_OPTIONS)
set(GLOBAL_FLAGS "${GLOBAL_FLAGS};${flags}")
get_target_property(flags MetFlags INTERFACE_COMPILE_OPTIONS)
set(GLOBAL_FLAGS "${GLOBAL_FLAGS};${flags}")
get_target_property(flags HGridFlags INTERFACE_COMPILE_OPTIONS)
set(GLOBAL_FLAGS "${GLOBAL_FLAGS};${flags}")
get_target_property(flags MiscFlags INTERFACE_COMPILE_OPTIONS)
set(GLOBAL_FLAGS "${GLOBAL_FLAGS};${flags}")
addFlagGroup(GlobalFlags "${GLOBAL_FLAGS}")