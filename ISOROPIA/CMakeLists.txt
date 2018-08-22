set(TARGET "Isoropia")
set(SOURCES 
    isorropiaII_main_mod.F
)
static_target(${TARGET} "${SOURCES}" "GlobalFlags")
target_compile_options(${TARGET} PRIVATE -r8)