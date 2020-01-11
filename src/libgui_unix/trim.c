static char sccs_id[] = "@(#)trim.c	1.1 6/3/94";
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
