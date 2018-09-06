# Substitute variables in CopyRunDirs.template:
set(UT_EXTDATA ${EXTDATA})
string(TOLOWER "${MET}" UT_MET)
string(REPLACE "_" "" UT_MET "${UT_MET}")
string(REPLACE "." "" UT_GRID "${GRID}")
set(UT_NEST "--")
string(TOLOWER "${MECH}" UT_MECH)
set(UT_START  "${START}")
set(UT_END "${END}")
set(UT_EXTRA "-")
configure_file(CopyRunDirs.template CopyRunDirs.input)


# Make a rundir target
add_custom_target(rundir ALL)

set(TEMP_RUNDIR "${CMAKE_CURRENT_BINARY_DIR}/${UT_MET}_${UT_GRID}_${UT_MECH}/")

add_custom_command(TARGET rundir
    POST_BUILD
    COMMAND rm -rf ${TEMP_RUNDIR}
    COMMAND perl gcCopyRunDirs ${CMAKE_CURRENT_BINARY_DIR}/CopyRunDirs.input > ${CMAKE_CURRENT_BINARY_DIR}/log || ( cat ${CMAKE_CURRENT_BINARY_DIR}/log & return 1 )
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/UT/perl/
    COMMENT "Running gcCopyRunDirs"
)
install(DIRECTORY
    ${TEMP_RUNDIR}
    DESTINATION ${RUNDIR}
    FILES_MATCHING 
        PATTERN "*.dat" 
        PATTERN "*.rc" 
        PATTERN "input.geos"
)