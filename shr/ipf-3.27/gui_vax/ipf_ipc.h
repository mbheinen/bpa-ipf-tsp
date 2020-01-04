/*********************************************
* file: ipc.h
* author: Dan Clark
* purpose: define ipc related functions and constants
*
**********************************************
*/
#ifndef TRUE
#define TRUE -1
#define FALSE 0
#endif

#define BUFSIZE 8192
#define FORT_BUFSIZE 8000
#define ENDOFMSG "*[EOM]"
#define SEPCHARS "\n"
#define SERIAL_NUMBER_SIZE 6 
#define I_OK			0 
#define I_ERROR			-1 
#define I_LOST_CONNECTION	-2 
#define I_NO_DATA		-4

#define I_SOCKID 1024

typedef char * Buffer;
#ifdef TESTBUF
typedef Buffer {
  char serialNumber[SERIAL_NUMBER_SIZE];
  char msg[BUFSIZE-SERIAL_NUMBER_SIZE];
};
#endif
