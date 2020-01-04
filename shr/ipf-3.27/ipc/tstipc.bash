#!/bin/bash
gcc -c ipc_inet.c
gcc -c ipc_com.c
gcc -c -DSERVER tstipc.c
mv tstipc.o srv.o
# gcc -o srv srv.o ipc_inet.o ipc_com.o -lsocket -lnsl
gcc -o srv srv.o ipc_inet.o ipc_com.o
gcc -c -DCLIENT tstipc.c
mv tstipc.o clt.o
# gcc -o clt clt.o ipc_inet.o ipc_com.o -lsocket -lnsl
gcc -o clt clt.o ipc_inet.o ipc_com.o
exit
