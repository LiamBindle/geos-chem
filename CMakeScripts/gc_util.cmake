function(stringify_list LIST)
    cmake_parse_arguments(BETTER
        "PRINT;AFTER" 
        "LINE_LENGTH" 
        "HIGHLIGHT;JOIN" 
        ${ARGN}
    )

    if(NOT DEFINED BETTER_LINE_LENGTH)
        set(BETTER_LINE_LENGTH 1000) # A big number
    endif()

    set(STR ${${LIST}})
    
    # Limit joined line length
    if(DEFINED BETTER_JOIN)
        set(TEMP "")
        set(CUR_LEN "0")

        set(JOIN_IDX "0 - 1")
        list(LENGTH BETTER_JOIN JOIN_LEN)
        
        foreach(ITEM ${STR})
            # Get the join token    
            math(EXPR JOIN_IDX "${JOIN_IDX} + 1")
            if(${JOIN_IDX} LESS ${JOIN_LEN})
                list(GET BETTER_JOIN "${JOIN_IDX}" JOIN_TOKEN)
            endif()
            string(LENGTH "${JOIN_TOKEN}" SEP_LEN)

            # If a line length was 
            string(LENGTH "${ITEM}" WORD_LEN)
            math(EXPR POST_LEN "${WORD_LEN} + ${CUR_LEN} + ${SEP_LEN}")
            if("${POST_LEN}" LESS "${BETTER_LINE_LENGTH}")
                if(${BETTER_AFTER})
                    set(TEMP "${TEMP}${ITEM}${JOIN_TOKEN}")
                else()
                    set(TEMP "${TEMP}${JOIN_TOKEN}${ITEM}")
                endif()
                set(CUR_LEN "${POST_LEN}")
            else()
                if(${BETTER_AFTER})
                    set(TEMP "${TEMP};${ITEM}${JOIN_TOKEN}")
                else()
                    set(TEMP "${TEMP};${JOIN_TOKEN}${ITEM}")
                endif()
                set(CUR_LEN "0")
                math(EXPR CUR_LEN "${SEP_LEN} + ${WORD_LEN}")
            endif()
        endforeach()

        set(STR "${TEMP}")
    endif()

    # Highlight selected words
    if(DEFINED BETTER_HIGHLIGHT)
        foreach(KEYWORD ${BETTER_HIGHLIGHT})
            string(REPLACE "${KEYWORD}" "[${KEYWORD}]" STR "${STR}")
        endforeach()
    endif()
    
    if(${BETTER_PRINT})
        string(ASCII 27 Esc)
        string(REGEX REPLACE "\\[([a-zA-Z0-9_\\.]+)\\]" "${Esc}[32m\\1${Esc}[m" COLORIZED "${STR}")
        message("${COLORIZED}")
    endif()

    
    # Export the new string
    set(${LIST} "${STR}" PARENT_SCOPE)
endfunction()

macro(dump_log LOG)
    if(DEFINED ${LOG})
        stringify_list(${LOG} 
            JOIN "\n" AFTER
            PRINT
        )
    endif()
endmacro()

function(warn_path_rules VAR LOG)
    cmake_parse_arguments(ENFORCE
        "EXISTS;PARENT;WRITABLE"
        ""
        "CONTAINS"
        ${ARGN}
    )

    # Make sure the path is absolute
    if(NOT IS_ABSOLUTE "${${VAR}}")
        set(${VAR} "${CMAKE_BINARY_DIR}/${${VAR}}")
        set(${VAR} "${${VAR}}" PARENT_SCOPE)
    endif()

    # Are we looking at the parent directory?
    if(${ENFORCE_PARENT})
        get_filename_component(${VAR} "${${VAR}}" DIRECTORY)
    endif()

    # Enforce path rules
    if(${ENFORCE_EXISTS})
        if(NOT EXISTS ${${VAR}})
            list(APPEND ${LOG} "${VAR} is invalid. ${${VAR}} does not exist!")
        endif()
    endif()

    if(${ENFORCE_WRITABLE})
        execute_process(COMMAND test -w ${${VAR}}
            RESULT_VARIABLE RC
        )
        if(${RC})
            list(APPEND ${LOG} "${VAR} is invalid. ${${VAR}} is not writable!")
        endif()
    endif()
    
    if(DEFINED ENFORCE_CONTAINS)
        foreach(FILE ${ENFORCE_CONTAINS})
            if(NOT EXISTS "${${VAR}}/${FILE}")
                list(APPEND ${LOG} "${VAR} is invalid. ${${VAR}}/${FILE} does not exist!")
            endif()
        endforeach()
    endif()
    set(${LOG} "${${LOG}}" PARENT_SCOPE)
endfunction()

function(set_dynamic_default VAR)
    cmake_parse_arguments(SDD
        ""
        "LOG"
        ""
        ${ARGN}
    )

    # Set flag indicating if ${VAR} can be set
    if(NOT DEFINED ${VAR} AND NOT DEFINED ${VAR}_IS_MUTABLE)
        set(${VAR}_IS_MUTABLE "true")
        set(${VAR}_IS_MUTABLE "true" PARENT_SCOPE)
    endif()

    # If ${VAR} is mutable, export it
    if(${${VAR}_IS_MUTABLE})
        set(${VAR} ${${VAR}} ${SDD_UNPARSED_ARGUMENTS})
        set(${VAR} ${${VAR}} PARENT_SCOPE)
    endif()

    # Log if requested
    if(DEFINED SDD_LOG)
        set(STR "${${VAR}}")

        # Split list with "  
        stringify_list(STR 
            JOIN "  " 
            LINE_LENGTH 60
        )
        # Wrap lines
        stringify_list(STR 
            JOIN "  + ${VAR}:\t" "\n  ...       \t"
        )
        list(APPEND ${SDD_LOG} "${STR}")
        set(${SDD_LOG} ${${SDD_LOG}} PARENT_SCOPE)
    endif()
endfunction()


function(set_dynamic_option VAR)
    cmake_parse_arguments(SDO  
        ""
        "SELECT_AT_LEAST;SELECT_AT_MOST;SELECT_EXACTLY;LOG" 
        "OPTIONS" 
        ${ARGN}
    )
    
    # Set/get value
    set_dynamic_default(${VAR} ${SDO_UNPARSED_ARGUMENTS})
    if(DEFINED SDO_LOG)
        set(STR "${SDO_OPTIONS}")

        # Split list with "  
        stringify_list(STR 
            JOIN "  " 
            LINE_LENGTH 60
            HIGHLIGHT ${${VAR}}
        )
        # Wrap lines
        stringify_list(STR 
            JOIN "  * ${VAR}:\t" "\n  ...       \t"
        )
        list(APPEND ${SDO_LOG} "${STR}")
        set(${SDO_LOG} ${${SDO_LOG}} PARENT_SCOPE)
    endif()

    # Count number of selected options
    set(SELECTED_COUNT 0)
    foreach(ITEM ${${VAR}})
        list(FIND SDO_OPTIONS "${ITEM}" FOUND)
        if(${FOUND} EQUAL -1)
            # A bad option was selected.
            string(REPLACE ";" ", " TEMP "${SDO_OPTIONS}")
            dump_log(${SDO_LOG})
            message(FATAL_ERROR "\"${ITEM}\" is not valid for ${VAR} (select from: ${TEMP})")
        else()
            math(EXPR SELECTED_COUNT "${SELECTED_COUNT} + 1")
        endif()
    endforeach()

    # Check AT_LEAST rule
    if(DEFINED SDO_SELECT_AT_LEAST)
        if(${SDO_SELECT_AT_LEAST} GREATER ${SELECTED_COUNT})
            dump_log(${SDO_LOG})
            message(FATAL_ERROR "At least ${SDO_SELECT_AT_LEAST} items must be selected for ${VAR}")
        endif()
    endif()

    # Check AT_MOST rule
    if(DEFINED SDO_SELECT_AT_MOST)
        if(${SELECTED_COUNT} GREATER ${SDO_SELECT_AT_MOST})
            dump_log(${SDO_LOG})
            message(FATAL_ERROR "At most ${SDO_SELECT_AT_MOST} items can be selected for ${VAR}")
        endif()
    endif()

    # Check EXACTLY rule
    if(DEFINED SDO_SELECT_EXACTLY)
        if(NOT ${SELECTED_COUNT} EQUAL ${SDO_SELECT_EXACTLY})
            stringify_list(MY_LOG PRINT)
            message(FATAL_ERROR "Exactly ${SDO_SELECT_EXACTLY} items must be selected for ${VAR}")
        endif()
    endif()

    # Export to parent scope
    set(${VAR} ${${VAR}} PARENT_SCOPE)
endfunction()