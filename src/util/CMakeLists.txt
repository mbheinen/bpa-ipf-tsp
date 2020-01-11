SET(IPFUTIL_src 
            ${SRCIPFUTIL}/c_err_exit.c
            ${SRCIPFUTIL}/c2f_wraps.c
            ${SRCIPFUTIL}/f2c_wraps.c
            ${SRCIPFUTIL}/ge_utils.c
            ${SRCIPFUTIL}/ipack_2.c
            ${SRCIPFUTIL}/ipack_4.c
            ${SRCIPFUTIL}/is_it_vms.c
            ${SRCIPFUTIL}/srv_cmdprs.c
)

ADD_LIBRARY(${IPFUTILLIB} STATIC ${IPFUTIL_src})
