# Add the source files
SET(PUF_src ${SRCPUF}/puf_main.c
            ${SRCPUF}/locint.c
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${PUFEXE} ${PUF_src})

TARGET_LINK_LIBRARIES(${PUFEXE} ${IPFLIB} ${IPFUTILLIB} ${IPCLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INSTALL(TARGETS ${PUFEXE} RUNTIME DESTINATION bin)
