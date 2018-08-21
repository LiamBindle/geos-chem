function(exe_target TARGET SRC_FILES PUBLIC_DEPS)
    string(REPLACE ";" " " SRC_FILES_HR "${SRC_FILES}")
    string(REPLACE ";" " " PUB_DEPS_HR  "${PUBLIC_DEPS}")
    message(STATUS "(X) Executable target: ${TARGET}")
    message(STATUS "    + Source files: ${SRC_FILES_HR}")
    message(STATUS "    + Dependencies: ${PUB_DEPS_HR}")

    # ${TARGET} is an executable
    add_executable(${TARGET}
        ${SRC_FILES}
    )

    # Declare dependencies of ${TARGET}
    target_link_libraries(${TARGET}
        PUBLIC ${PUBLIC_DEPS}
    )

    # Install ${TARGET} to ${CMAKE_INSTALL_PREFIX}/bin
    install(TARGETS ${TARGET}
        RUNTIME DESTINATION bin
    )
endfunction(exe_target)

function(shared_target TARGET SRC_FILES PUBLIC_DEPS)
    string(REPLACE ";" " " SRC_FILES_HR "${SRC_FILES}")
    string(REPLACE ";" " " PUB_DEPS_HR  "${PUBLIC_DEPS}")
    message(STATUS "(S) Shared library target: ${TARGET}")
    message(STATUS "    + Source files: ${SRC_FILES_HR}")
    message(STATUS "    + Dependencies: ${PUB_DEPS_HR}")

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
endfunction(shared_target)

function(static_target TARGET SRC_FILES)
    string(REPLACE ";" " " SRC_FILES_HR "${SRC_FILES}")
    string(REPLACE ";" " " PUB_DEPS_HR  "${PUBLIC_DEPS}")
    message(STATUS "(A) Archive target: ${TARGET}")
    message(STATUS "    + Source files: ${SRC_FILES_HR}")
    message(STATUS "    + Dependencies: ${PUB_DEPS_HR}")

    # ${TARGET} is a static library
    add_library(${TARGET} STATIC 
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

    # Install ${TARGET} to ${CMAKE_INSTALL_PREFIX}/lib
    install(TARGETS ${TARGET}
        ARCHIVE DESTINATION lib
    )

    # Install modules in ./include to ${CMAKE_INSTALL_PREFIX}/include
    install(DIRECTORY include/
        DESTINATION include
    )
endfunction(shared_target)