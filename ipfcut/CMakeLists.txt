# Add the source files
SET(IPFCUT_src ${SRCIPFCUT}/ipfcut)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${IPFCUTEXE} ${IPFCUT_src})

TARGET_LINK_LIBRARIES(${IPFCUTEXE} ${IPFLIB} ${IPCLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INSTALL(TARGETS ${IPFCUTEXE} RUNTIME DESTINATION bin)
