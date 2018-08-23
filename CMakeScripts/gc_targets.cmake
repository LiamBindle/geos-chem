function(print_list MY_LIST)
    foreach(ITEM ${MY_LIST})
    message("         + ${ITEM}")
    endforeach(ITEM)
endfunction(print_list)

function(exe_target TARGET SRC_FILES PUBLIC_DEPS)
    message(STATUS "(X) Executable target: ${TARGET}")
    message("       Builds: ${TARGET}")
    message("       Source directory: ${CMAKE_CURRENT_SOURCE_DIR}")
    message("       Source files:")
    print_list(${SRC_FILES})
    message("       Dependencies:")
    print_list(${PUBLIC_DEPS})

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
    message(STATUS "(S) Shared library target: ${TARGET}")
    message("       Builds: lib${TARGET}.so")
    message("       Source directory: ${CMAKE_CURRENT_SOURCE_DIR}")
    message("       Source files:")
    print_list(${SRC_FILES})
    message("       Dependencies:")
    print_list(${PUBLIC_DEPS})

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
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/
        DESTINATION include
    )
endfunction(shared_target)

function(static_target TARGET SRC_FILES PUB_DEPS)
    message(STATUS "(A) Archive target: ${TARGET}")
    message("       Builds: lib${TARGET}.a")
    message("       Source directory: ${CMAKE_CURRENT_SOURCE_DIR}")
    list(LENGTH SRC_FILES NUM_SRC_FILES)
    message("       Source files: ${NUM_SRC_FILES}")
    message("       Dependencies:")
    print_list("${PUB_DEPS}")

    # ${TARGET} is a static library
    add_library(${TARGET} STATIC 
        ${SRC_FILES}
    )

    # Module of ${TARGET} will be put in ./include
    set_target_properties(${TARGET}
        PROPERTIES 
            Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
    )

    # Add public dependencies
    target_link_libraries(${TARGET}
        PUBLIC ${PUB_DEPS}
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
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/
        DESTINATION include
    )
endfunction(static_target)