set(TARGET "HCOI")
set(SOURCES 
    hcoi_esmf_mod.F90
    hcoi_standalone_mod.F90
)
static_target(${TARGET} "${SOURCES}" "HCOX" "")