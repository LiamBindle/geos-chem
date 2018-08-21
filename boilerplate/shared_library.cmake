# Set target (shared libary) name
set(TARGET      "mysharedlib")
# Declare source files of ${TARGET}
set(SRC_FILES   "myfile1.f myfile2.f myfile3.f")
# Declare dependencies (other targets) of ${TARGET}
set(PUBLIC_DEPS "othertarget1 othertarget2")


# ${TARGET} is a shared library
add_library(${TARGET} SHARED 
    ${SRC_FILES}
)

# Module of ${TARGET} will be put in ./include
set_target_properties(${TARGET}
    PROPERTIES 
        Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
)

# Future targets linking-in ${TARGET} must include ./include (i.e. the modules)
target_include_directories(${TARGET}
    PUBLIC
        $<INSTALL_INTERFACE:include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
)

# Declare dependencies of ${TARGET}
target_link_libraries(${TARGET}
    PUBLIC ${PUBLIC_DEPS}
)

# Install ${TARGET} to ${CMAKE_INSTALL_PREFIX}/lib
install(TARGETS ${TARGET}
    LIBRARY DESTINATION lib
)

# Install modules in ./include to ${CMAKE_INSTALL_PREFIX}/include
install(DIRECTORY include/
    DESTINATION include
)