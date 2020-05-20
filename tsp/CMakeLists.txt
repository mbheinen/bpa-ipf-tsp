# Add the source files
SET(TSP_src ${SRCTSP}/main_tsp.c
            ${SRCTSP}/set_exit.c
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${TSPEXE} ${TSP_src})

TARGET_LINK_LIBRARIES(${TSPEXE} ${TSPLIB} ${IPFLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INSTALL(TARGETS ${TSPEXE} RUNTIME DESTINATION bin)
