SET(IPC_src 
            ${SRCIPC}/ipc_com.c
            ${SRCIPC}/ipc_inet.c
)
INCLUDE_DIRECTORIES(${SRC}/include)

ADD_LIBRARY(${IPCLIB} STATIC ${IPC_src})
