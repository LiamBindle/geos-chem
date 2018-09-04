# MET field
set_dynamic_option(MET "GEOS_FP"
    LOG GENERAL_OPTIONS_LOG
    SELECT_EXACTLY 1
    OPTIONS "GEOS_FP" "MERRA2"
)
set_dynamic_default(GC_DEFINES ${MET})

# Check for nested grid
set_dynamic_option(NESTED "FALSE"
    LOG GENERAL_OPTIONS_LOG
    SELECT_EXACTLY 1
    OPTIONS "TRUE" "FALSE"
)
if(${NESTED})
    # Which nested grid?
    set_dynamic_option(REGION "NA"
        LOG GENERAL_OPTIONS_LOG
        SELECT_EXACTLY 1
        OPTIONS "AS" "CH" "CU" "EU" "NA" 
    )
    set_dynamic_default(GC_DEFINES NESTED NESTED_${REGION})
endif()

# Horizontal grid
if(${NESTED})
    if("${MET}" STREQUAL "MERRA2") # Nested w/ MERRA2 
        set_dynamic_option(GRID "0.5x0.625"
            LOG GENERAL_OPTIONS_LOG
            SELECT_EXACTLY 1
            OPTIONS "0.5x0.625"
        )
    else() # Nested w/ GEOS_FP
        set_dynamic_option(GRID "0.25x0.3125"
            LOG GENERAL_OPTIONS_LOG
            SELECT_EXACTLY 1
            OPTIONS "0.25x0.3125"
        )
    endif()
else() # Not nested
    set_dynamic_option(GRID "4x5"
        LOG GENERAL_OPTIONS_LOG
        SELECT_EXACTLY 1
        OPTIONS "4x5" "2x2.5"
    )
endif()
string(REPLACE "." "" TEMP "GRID${GRID}")
set_dynamic_default(GC_DEFINES ${TEMP})

# Chemistry mechanism
set_dynamic_option(MECH "Standard"
    LOG GENERAL_OPTIONS_LOG
    SELECT_EXACTLY 1
    OPTIONS "Standard" "Tropchem" "SOA_SVPOA"
)

message(STATUS "General settings:")
dump_log(GENERAL_OPTIONS_LOG)


# Get diagnostics
set_dynamic_default(DIAG 
    "BPCH_DIAG" "BPCH_TIMESER" "BPCH_TPBC"

    LOG EXTRA_DEFS_LOG
)
set_dynamic_default(GC_DEFINES ${DIAG})


# Get extra defines
set_dynamic_default(EXTRA 
    "UCX" "USE_REAL8" "USE_TIMERS"
    
    LOG EXTRA_DEFS_LOG
)
set_dynamic_default(GC_DEFINES ${EXTRA})

message(STATUS "Additional definitions:")
dump_log(EXTRA_DEFS_LOG)

# Get resulting GC_DEFINES
string(REPLACE " " ";" GC_DEFINES "${GC_DEFINES}")
set_dynamic_default(GC_DEFINES LOG RESULTING_DEFINES_LOG)

# Get compiler options
set_dynamic_default(FC_OPTIONS
    -fPIC -cpp -w -auto -noalign "-convert big_endian" -O2 -vec-report0 
    "-fp-model source" -openmp -mcmodel=medium -shared-intel -traceback
    -DLINUX_IFORT

    LOG RESULTING_DEFINES_LOG
)

message(STATUS "Resulting definitions/options:")
dump_log(RESULTING_DEFINES_LOG)

# Replace ';' character (delimiting lists) with ' '
string(REPLACE ";" " " GC_DEFINES "${GC_DEFINES}")
string(REPLACE ";" " " FC_OPTIONS "${FC_OPTIONS}")
