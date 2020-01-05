static char sccsid[]="@(#)fixvuit.c	20.2 2/15/95";
/*
*           IPF V2.x: Interactive PowerFlow Version 2.x
*              Bonneville Power Administration (BPA)
*         Authors:  D. L. Clark, J. G. Coleman, W. L. Powell, 
*                B. E. Rogers, K. E. Rowell, J. L. Rutis, D. M. Stefonik, 
*                D. M. Syzmanski 
*
*                              NOTICE
*
* Interactive Powerflow (IPF) was developed by BPA and its contractors
* with about 20% of the cost supported by the Electric Power Research
* Institute (EPRI).  By mutual agreement, as described in EPRI 
* Agreement RP2746-03 entitled Graphical User Interface for 
* Powerflow, March, 1992, all results of this project--including 
* the computer program and its documentation--are to be in the 
* public domain.  In a separate Memorandum of Understanding with the 
* Western Systems Coordinating Council (WSCC), BPA agreed in March, 1992, 
* to keep WSCC informed of progress, to make its best effort to develop 
* the program according to the Guidelines adopted by the WSCC Computer 
* Program Management Subcommittee, and to make the final results 
* available for possible further development by WSCC. 
*
* This notice must appear in all copies of this software and documentation.
*/
#include <stdio.h>

#ifndef DEBUG
#define DEBUG 0
#endif

main(int argc, char *argv[])
{
  char *pfilenme;
  char *ppgm;
  FILE *pfile;
  FILE *pfileout;
#define MAXNAME 128
  char newname[MAXNAME];
#define MAXLINE 512
#define LINELIMIT 128
  char line[MAXLINE];
  char nxtline[MAXLINE];
  char *retline;
  int endofline;
  int nxtendofline;
  int cnt;


  ppgm = argv[0];

  if(argc < 2) {
    printf("Usage %s <filename>\n", ppgm);
    return 1;
  }
  pfilenme = argv[1];

  printf("Pgm %s operating on file %s\n", ppgm, pfilenme);

  if ((pfile = fopen(pfilenme, "r") ) == NULL ) {
    printf("Failed opening file %s\n", pfilenme);
    return 1;
  }
  strncpy(newname, pfilenme, MAXNAME - 3);
  strcat(newname, "fix");
  if ((pfileout = fopen(newname, "w") ) == NULL ) {
    printf("Failed opening file %s\n", newname);
    return 1;
  }

  cnt = 0;
  while (( retline = fgets(line, MAXLINE, pfile) ) != NULL ) {
    /* more lines to process */
    endofline = strlen(line);
    if(strncmp(&line[endofline-2], "\\", 1) == 0 ) {
      if(DEBUG) printf("long line: %s", line);  
      if(endofline < LINELIMIT - 1) {
         printf("WARNING - short line %d %s\n", cnt, line);
      } else {
        /** check the next line to see if it is the last icon line **/
        if (( retline = fgets(nxtline, MAXLINE, pfile) ) == NULL ) {
          printf("WARNING - ran out of lines in file\n");
          break;
        }
        nxtendofline = strlen(nxtline);
        if(strncmp(&nxtline[nxtendofline-2], ";", 1) == 0 ) {
           /* no comma on last icon line */
           strncpy(&line[endofline-3], "'", 1);
           strncpy(&line[endofline-2], " ", 1);
           if(DEBUG) printf("new  line: %s", line);
           fputs(line, pfileout);
           strcpy( nxtline, ");\n" );
           fputs(nxtline, pfileout);
        } else {
           /* commas after each icon line */
           strncpy(&line[endofline-3], "'", 1);
           strncpy(&line[endofline-2], ",", 1);
           fputs(line, pfileout);
        }

      }
    } else {
      /** just copy over line to new file **/
      fputs(line, pfileout);
    }
    cnt++;
  }
  
  fclose(pfileout);
  fclose(pfile);

}

