static char sccs_id[] = "@(#)trim.c	1.4 2/15/95";
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
/*
* purpose: take all the trailing spaces off the lines in the file
*  to facilitate comparing files corrupted by external programs
* author: Dan Clark	Jun 94
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
main(int argc, char *argv[])
{
   FILE *fptri;
   FILE *fptro;
   char filename[1024];
   char buf[1024];
   char *last;
   int i;

   if(argc < 2) {
      printf("usage: %s <filename>\n", argv[0]);
      exit(1);
   }

   for(i = 1; i < argc; i++) {
      sprintf(filename, "%s", argv[i]);
      if(NULL == (fptri = fopen(filename, "r"))) {
         printf("Error - cannot open file %s\n", filename);
         exit(1);
      }
      sprintf(filename, "new/%s", argv[i]);
      if(NULL == (fptro = fopen(filename, "w"))) {
         printf("Error - cannot open file %s\n", filename);
         exit(1);
      }

      memset(buf, '\0', sizeof(buf));
      while(fgets(buf, sizeof(buf), fptri) != NULL) {
         last = strchr(buf, '\n');
         *last = '\0';
         last--;
         while(*last == ' ') {
            *last = '\0';
            last--;
         }
         last++;
         *last = '\n';
         fputs(buf, fptro);
         memset(buf, '\0', sizeof(buf));
      }

      fclose(fptri);
      fclose(fptro);

   } /* end of loop through files */
}
