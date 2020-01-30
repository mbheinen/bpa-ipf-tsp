/*****************************************************************
* File: prs_cmdfil.c
* Purpose: Routine to parse a command file into commands, then
*          call "srv_cmdprs" for each command.
*
* Command file execution does work OK and even allows for nested command
* files.  In terms of error message reporting, however, the implementation
* is some what lacking for interactive use (GUI or CFLOW).  The problem is
* that, for non-fatal errors, any contents of "outbuf" (including any non-
* fatal warning or error messages) returned by the individual commands in
* the command file are overwritten with each new command, the last command
* is overwritten by the status info from the "/COMMAND" command itself.
* The current design terminates command file execution (all nestings), if
* a command returns a status other than zero.  This allows the error
* messages and return status of the failed command to be returned.  The re-
* design to allow all error messages (including warnings, etc.) to be
* returned to the GUI affects about 200 modules, so, will be done as
* time/priority permits.
*
*****************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "ipf_ipc.h"

#ifdef WIN32
#pragma aux apnd_stat_cw "^"
#endif

/* prototypes */
int srv_cmdprs( char *, char * );
int prs_cmdfil( char *, char *, char * );
void apnd_stat_cw( char *, int, char * );

#define MAX_OBUF   FORT_BUFSIZE - 50
#define MAX_IBUF   FORT_BUFSIZE - 10

int p_cmdfil_c( char *inbuf, char *outbuf )
{
   static char  rname[16] = "p_cmdfil_c.c";
   char  filnam[61], ertext[133];
   int   cmdstat;
   char  *p, *p1, *p2;

   cmdstat = 0;     /* default return SUCCESS state */
   /* parse -- "/command, file = filename" to get filename */
   memset( filnam, '\0', sizeof( filnam ) );
   p1 = strchr( inbuf, '\n' );
   p = strtok( inbuf, ", =\n\0" );
   p = strtok( 0, ", =\n\0" );     /* skip "/command"  */
   while ( p < p1  &&  p != 0 ) {
      for ( p2=p; *p2 != '\0'; p2++ ) *p2 = (char) toupper( (int) *p2 );
      if ( strncmp( p, "FILE", 4 ) == 0 ) {
         p = strtok( 0, ", =\n\0" );
         if ( p != 0 ) strcpy( filnam, p );
      }
      p = strtok( 0, ", =\n\0" );
   }
   cmdstat = prs_cmdfil( filnam, inbuf, outbuf );
   if ( cmdstat == 0 ) apnd_stat_cw( outbuf, cmdstat, rname );
   return cmdstat;
}


int prs_cmdfil( char *filnam, char *inbuf, char *outbuf )
{
   static char  rname[16] = "prs_cmdfil.c";
   FILE  *inpx;
   char  text[133], ertext[133], cmdfilbuf[BUFSIZE];
   int   cmdstat, char_cnt;
   char  *p, *p1, *p2;

   cmdstat = 0;     /* default return SUCCESS state */
   inpx = fopen( filnam, "r" );
   if ( inpx == NULL ) {
      sprintf( outbuf,
         " *** FATAL    Command file  %s  could not be opened\n\0", filnam );
      cmdstat = 1;
      apnd_stat_cw( outbuf, cmdstat, rname );
      return cmdstat;
   }
   while ( ( p = fgets( text, sizeof(text)-1, inpx ) ) != NULL ) {
      if ( *p == '/' ) break;
   }
   if ( p == NULL ) {
      sprintf( outbuf,
         " *** FATAL    Error reading command file  %s \n\0", filnam );
      fclose( inpx );
      cmdstat = 1;
      apnd_stat_cw( outbuf, cmdstat, rname );
      return cmdstat;
   }
   strcpy( inbuf, p );
   char_cnt = strlen(p);
   while ( ( p = fgets( text, sizeof(text)-1, inpx ) ) != NULL ) {
      if ( *p == '.'  ||  *p == '(' ) continue;
      if ( *p == '/' ) {
         strcat( inbuf, "*[EOM]\0" );
         cmdstat = srv_cmdprs( inbuf, cmdfilbuf );
         if ( cmdstat != 0 ) {
            fclose( inpx );
            strcpy( outbuf, cmdfilbuf );
            return cmdstat;
         }
         *inbuf = '\0';
         char_cnt = 0;
      }
      char_cnt += strlen(p);
      if ( char_cnt < MAX_IBUF ) {
         strcat( inbuf, p );
      } else {
         sprintf( outbuf,
            " *** FATAL    Command buffer overflow, command file =  %s \0",
                  filnam );
         sprintf( ertext, ", Command line =\n *** FATAL    %s\0", p );
         strcat( outbuf, ertext );
         fclose( inpx );
         cmdstat = 1;
         apnd_stat_cw( outbuf, cmdstat, rname );
         return cmdstat;
      }
   }
   /* take care of last command */
   strcat( inbuf, "*[EOM]\0" );
   cmdstat = srv_cmdprs( inbuf, cmdfilbuf );
   if ( cmdstat != 0 ) strcpy( outbuf, cmdfilbuf );
   /*
    *    NO return status text if everything OK;
    *    "p_cmdfil_c" handles this case.
    *    (NO call to "apnd_stat_cw")
    */
   fclose( inpx );
   return cmdstat;
}

