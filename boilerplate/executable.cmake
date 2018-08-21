# Set target (shared libary) name
set(TARGET      "myexe")
# Declare source files of ${TARGET}
set(SRC_FILES   "main.f")
# Declare dependencies (other targets) of ${TARGET}
set(PUBLIC_DEPS "othertarget1 othertarget2")


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