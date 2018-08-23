# TODO: import env vars from Makefile
set(TARGET "kpp_firstpass")
set(SOURCES
    gckpp_Precision.F90
    gckpp_Parameters.F90
    gckpp_Monitor.F90
)
static_target(${TARGET} "${SOURCES}" "")
set(KPP_FIRST_PASS_COMPLETED "TRUE")

set(TARGET "Kpp")
set(SOURCES
    gckpp_Function.F90
    gckpp_Global.F90
    gckpp_HetRates.F90
    gckpp_Initialize.F90
    gckpp_Integrator.F90
    gckpp_Jacobian.F90
    gckpp_JacobianSP.F90
    gckpp_LinearAlgebra.F90
    gckpp_Model.F90
    gckpp_Monitor.F90
    gckpp_Parameters.F90
    gckpp_Precision.F90
    gckpp_Rates.F90
    gckpp_Util.F90
)
static_target(${TARGET} "${SOURCES}" "GeosUtil2")
