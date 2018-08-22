set(TARGET "GeosUtil")
set(SOURCES

    henry_mod.F
    ifort_errmsg.F
    julday_mod.F
)
static_target(${TARGET} "${SOURCES}" "Headers")