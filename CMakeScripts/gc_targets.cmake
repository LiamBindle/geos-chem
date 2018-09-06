function(print_list MY_LIST)
    foreach(ITEM ${MY_LIST})
    message("         + ${ITEM}")
    endforeach(ITEM)
endfunction(print_list)

function(exe_target TARGET SRC_FILES PUBLIC_DEPS)
    list(LENGTH SRC_FILES NUM_SRC_FILES)
    list(LENGTH PUBLIC_DEPS NUM_PUBLIC_DEPS)
    message(STATUS "Executable target: ${TARGET} (s:${NUM_SRC_FILES}/d:${NUM_PUBLIC_DEPS})")

    # ${TARGET} is an executable
    add_executable(${TARGET}
        ${SRC_FILES}
    )

    # Declare dependencies of ${TARGET}
    target_link_libraries(${TARGET}
        PUBLIC ${PUBLIC_DEPS}
    )

    # Install ${TARGET} to ${CMAKE_INSTALL_PREFIX}
    install(TARGETS ${TARGET}
        RUNTIME DESTINATION .
    )
endfunction(exe_target)

function(shared_target TARGET SRC_FILES PUBLIC_DEPS)
    list(LENGTH SRC_FILES NUM_SRC_FILES)
    list(LENGTH PUBLIC_DEPS NUM_PUBLIC_DEPS)
    message(STATUS "Shared library target: lib${TARGET}.so (s:${NUM_SRC_FILES}/d: ${NUM_PUBLIC_DEPS})")

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

    # # Install ${TARGET} to ${CMAKE_INSTALL_PREFIX}/lib
    # install(TARGETS ${TARGET}
    #     LIBRARY DESTINATION lib
    # )

    # # Install modules in ./include to ${CMAKE_INSTALL_PREFIX}/include
    # install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/
    #     DESTINATION include
    # )
endfunction(shared_target)

function(static_target TARGET SRC_FILES PUB_DEPS)
    list(LENGTH SRC_FILES NUM_SRC_FILES)
    list(LENGTH PUB_DEPS NUM_PUBLIC_DEPS)
    message(STATUS "Static library target: lib${TARGET}.a (s:${NUM_SRC_FILES}/d:${NUM_PUBLIC_DEPS})")

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

    # # Install ${TARGET} to ${CMAKE_INSTALL_PREFIX}/lib
    # install(TARGETS ${TARGET}
    #     ARCHIVE DESTINATION lib
    # )

    # # Install modules in ./include to ${CMAKE_INSTALL_PREFIX}/include
    # install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/
    #     DESTINATION include
    # )
endfunction(static_target)