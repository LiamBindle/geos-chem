set(TARGET "kpp_firstpass")
set(SOURCES
    gckpp_Precision.F90
    gckpp_Parameters.F90
    gckpp_Monitor.F90
)
static_target(${TARGET} "${SOURCES}" "" "")