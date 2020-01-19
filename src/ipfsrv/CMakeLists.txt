# Add the source files
SET(IPFSRV_src ${SRCIPFSRV}/ipfsrv.c
               ${SRCIPFSRV}/ipfsrv_sub.c
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${IPFSRVEXE} ${IPFSRV_src})

TARGET_LINK_LIBRARIES(${IPFSRVEXE} ${IPFLIB} ${IPFUTILLIB} ${IPCLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INCLUDE_DIRECTORIES(${SRCIPF})
INSTALL(TARGETS ${IPFSRVEXE} RUNTIME DESTINATION bin)
