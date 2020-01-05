/*************************************************************************\
* PURPOSE:  minor review program to check the procs.u file.
*
*	Reads the "procs.u" file and generates list of all procedures.
*
*	Note: other procedures exists in other *.u files
*
*
\***************************************************************************/
#include <stdio.h>
int uscanc (char *in, char *out[], int *nwrd, char *limit1, char *limit2);

main()
{
  char str[200];
  char stars[]="***";
  char *user_prc[300];
  char *proc_nam[300];
  char *man_proc[300];
  char *unm_proc[300];
  char *hold;
  int i,n,nproc, nwrd, numn, nman;
  char *words[20];
  char *limit1 = ",=", *limit2 = " \t\n";
 

  FILE *fptr;

  if(( fptr = fopen("procs.u", "r" )) == 0 )
  {
     printf("\n ================================================\n");
     printf(  " =** UNABLE TO OPEN PROCS.U FILE              **=\n");
     printf(  " ================================================\n\n");
  }
 
  n = 0;
  nproc = 0;
  
  while ((fgets( str, 132, fptr )) != NULL)
  {
    n++; 

    if( strncmp( str, "!***VUIT_no_Generate***", 23 ) == 0 )
    {
      fgets( str, 132, fptr );
      n++; 
      if ( strncmp( str, "!***VUIT_Action", 15 ) != 0 )
      {
	printf("***WARNING*** VUIT_no_Generate card was not followed by a VUIT_Action card at line %d\n%s\n", n, str );
      }
    }



    if ( strncmp( str, "!***VUIT_Action", 15 ) == 0 )
    {
      uscanc (str, words, &nwrd, limit1, limit2);
/*      printf("%s %s %s\n", words[0], words[1], words[2] ); */
  
      if( strcmp( words[1], "UserProc" ) == 0 )
      {
 	if( strcmp( words[2], "***" ) != 0 )
	{
	  user_prc[++nproc] = words[2];

	  /* now find the name */
	  fgets( str, 132, fptr );
	  fgets( str, 132, fptr );	/*skip blank*/
	  uscanc (str, words, &nwrd, limit1, limit2);
	  if( strncmp( words[0], user_prc[nproc], 5 ) != 0 )
	    proc_nam[nproc] = words[0];
	  else
	    proc_nam[nproc] = &stars[0];
	}
	else
	{
	  fgets( str, 132, fptr );
	  n++; 
	  fgets( str, 132, fptr );
	  n++; 
	  if( str[1] != '!' ) 
	  {
	    uscanc (str, words, &nwrd, limit1, limit2);
	    user_prc[++nproc] = words[0];
	    proc_nam[nproc] = &stars[0];
	  }
	}
      }

    /**************************************************************************/
      else if( strcmp( words[1], "Manage" ) == 0 )
      {
	man_proc[++nman] = words[2];
      }

      else if( strcmp( words[1], "Unmanage" ) == 0 )
      {
	unm_proc[++numn] = words[2];
      }
      else
      {
	printf("UNKNOWN %s\n", str );
      }
    }
  }

  i=0;
  while( ++i<= nman )
  {
    int j;
    j = i;
    while( ++j<= nman )
    {
      if( strcmp( man_proc[j], man_proc[i] ) < 0 )
      {
	hold = man_proc[j];
	man_proc[j] = man_proc[i];
	man_proc[i] = hold;
      }
    }
    printf("MANAGE %s\n", man_proc[i] );
  }

  printf("\n\n");

  i=0;
  while( ++i<= numn )
  {
    int j;
    j = i;
    while( ++j<= numn )
    {
      if( strcmp( unm_proc[j], unm_proc[i] ) < 0 )
      {
	hold = unm_proc[j];
	unm_proc[j] = unm_proc[i];
	unm_proc[i] = hold;
      }
    }
    printf("UNMANAGE %s\n", unm_proc[i] );
  }

/***************************************************************************\
********  SORT *************************************************************/

  printf("\f           C-ROUTINE TO CALL:        OFFICAL VUIT NAME:\n");

  i=0;
  while( ++i<= nproc )
  {
    int j;
    j = i;
    while( ++j<= nproc )
    {
      if( strcmp( user_prc[j], user_prc[i] ) < 0 )
      {
	hold = user_prc[j];
	user_prc[j] = user_prc[i];
	user_prc[i] = hold;
	hold = proc_nam[j];
	proc_nam[j] = proc_nam[i];
	proc_nam[i] = hold;
      }
    }
    printf("%3d %30.30s     %s\n", i, user_prc[i], proc_nam[i] );
  }
}



int uscanc (char *in, char *out[], int *nwrd, char *limit1, char *limit2)
{
   char *c, *d;
   int num = 0;

   num = 0;

   c = in;
   out[num] = (char *) malloc(strlen(c)+1);         /* allocate new word with
                                                       entire string c */
   strcpy(out[num], c);
   d = out[num];
   for (c = in; *c != 0; c++) {
      if ( strchr(limit1, *c) != NULL) {            /* parse delimiter */
         *d = '\0';                                 /* delimit last word */
         if (strlen(out[num]) > 0) num++;
         out[num] = (char *) malloc(2);             /* allocate 2 bytes for
                                                       visible delimiter */
         strcpy(out[num], strchr(limit1, *c));      /* copy delimiter */
         num++;
         out[num] = (char *) malloc(strlen(c)+1);   /* allocate new word with
                                                       remaining string c */
         strcpy(out[num], c);
         d = out[num];
      } else if ( strchr(limit2, *c) != NULL) {     /* skip delimiter */
         *d = '\0';                                 /* delimit last word */
         if (strlen(out[num]) > 0) num++;
         out[num] = (char *) malloc(strlen(++c)+1); /* allocate new word with
                                                       remaining string c */
         strcpy(out[num], c--);
         d = out[num];
      } else {
         d++;                                       /* character accepted in
                                                       out[num];
                                                       advance pointer */
      }
   }
   if (strlen(out[num]) > 0) num++;
   *nwrd = num;

   return(0);
}

