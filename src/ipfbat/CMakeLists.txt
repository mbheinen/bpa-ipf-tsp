# Add the source files
SET(IPFBAT_src ${SRCIPFBAT}/ipfbat.c
               ${SRCIPFBAT}/ipfbat_sub.c
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${IPFBATEXE} ${IPFBAT_src})

TARGET_LINK_LIBRARIES(${IPFBATEXE} ${IPFLIB} ${IPCLIB} ${IPFUTILLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INCLUDE_DIRECTORIES(${SRCIPC})
INSTALL(TARGETS ${IPFBATEXE} RUNTIME DESTINATION bin)
