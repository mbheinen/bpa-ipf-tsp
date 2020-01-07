SET(IPC_src 
            ${SRCIPC}/ipc_com.c
            ${SRCIPC}/ipc_inet.c
)

ADD_LIBRARY(${IPCLIB} STATIC ${IPC_src})
