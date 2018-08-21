set(TARGET  "Headers")
set(SOURCES

    charpak_mod.F90
    inquireMod.F90
    errcode_mod.F90
    precision_mod.F
    physconstants.F
    registry_params_mod.F90
    registry_mod.F90
    diaglist_mod.F90
    input_opt_mod.F90
    species_mod.F90
    species_database_mod.F90
    state_chm_mod.F90
    state_diag_mod.F90
    CMN_SIZE_mod.F
    CMN_FJX_MOD.F
    CMN_DIAG_mod.F
    CMN_O3_mod.F
    state_met_mod.F90
)
static_target(${TARGET} "${SOURCES}" "kpp_firstpass" "")