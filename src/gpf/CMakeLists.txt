# Add the source files
SET(GPF_src ${SRCGPF}/ipfmain.c)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${GPFEXE} ${GPF_src})

TARGET_LINK_LIBRARIES(${GPFEXE} ${IPFLIB} ${IPFUTILLIB} ${IPCLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INSTALL(TARGETS ${GPFEXE} RUNTIME DESTINATION bin)
