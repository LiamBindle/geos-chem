set(TARGET "GeosUtil2")
set(SOURCES

    regrid_a2a_mod.F90
    roundoff_mod.F90
    geos_timers_mod.F90
    error_mod.F
    gc_grid_mod.F90
    pressure_mod.F
    grid_registry_mod.F90
    unitconv_mod.F90
    file_mod.F
    bpch2_mod.F
    hdf_mod.F
    time_mod.F
    transfer_mod.F
)
static_target(${TARGET} "${SOURCES}" "GeosUtil;nc" "")