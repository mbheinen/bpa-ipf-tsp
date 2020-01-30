# Add the source files
SET(NETDAT_src ${SRCNETDAT}/net_data.f)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${NETDATEXE} ${NETDAT_src})

TARGET_LINK_LIBRARIES(${NETDATEXE} ${IPFLIB} ${IPCLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INSTALL(TARGETS ${NETDATEXE} RUNTIME DESTINATION bin)
