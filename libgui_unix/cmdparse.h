/*******************************************
* file: ipcparse.h
* author: Dan Clark
* date: Feb, 1992
* purpose: 

* revised:
*  Apr 20, 1992	Dan Clark
*   add column of call up routines for the gui parsing of
*   the return command from powerflow.
*   and a colum of call back routines for the gui.
*******************************************
*/

#define IPC_CMD_LENGTH 32

/*** 
* structure of command table shared between client and server 
* command - a character string of up to IPC_CMD_LENGTH characters
*  variable length.
* fptr - a function pointer to the server routine to create the data
*  which will be passed back to the client.
***/
typedef struct cmdTbl {
   char command[IPC_CMD_LENGTH]; 
   int  (*fptr)();
} CmdTbl;
