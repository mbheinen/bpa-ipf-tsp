# Add the source files
SET(BPF_src ${SRCBPF}/bpf_main.c
            ${SRCBPF}/bpf_sub.c
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${BPFEXE} ${BPF_src})

TARGET_LINK_LIBRARIES(${BPFEXE} ${IPFLIB} ${IPFUTILLIB} ${IPCLIB})

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INSTALL(TARGETS ${BPFEXE} RUNTIME DESTINATION bin)
