SET(CFLOW_src 
            ${SRCLIBCFLOW}/cflow_pf_init.c
            ${SRCLIBCFLOW}/cflow_lib_ipc.c
            ${SRCLIBCFLOW}/convtbl.c
            ${SRCLIBCFLOW}/ft.c
            ${SRCLIBCFLOW}/pf_rec.c
            ${SRCLIBCFLOW}/run_ipfsrv.c
)
INCLUDE_DIRECTORIES(../include)
ADD_LIBRARY(${CFLOWLIB} STATIC ${CFLOW_src})
