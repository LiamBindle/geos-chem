set(ISOROPIA_TARGET "Isoropia")
set(ISOROPIA_TARGET "${ISOROPIA_TARGET}" PARENT_SCOPE)
set(SOURCES 
    isorropiaII_main_mod.F
)
static_target(${ISOROPIA_TARGET} "${SOURCES}" "")
target_compile_options(${ISOROPIA_TARGET} PRIVATE -r8)
target_compile_definitions(${ISOROPIA_TARGET}
    PUBLIC ${GC_DEFINES}
)