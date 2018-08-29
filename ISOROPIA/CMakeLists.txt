set(TARGET "Isoropia")
set(SOURCES 
    isorropiaII_main_mod.F
)
static_target(${TARGET} "${SOURCES}" "")
target_compile_options(${TARGET} PRIVATE -r8)
target_compile_definitions(${TARGET}
    PUBLIC ${GC_DEFINES}
)