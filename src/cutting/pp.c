/*************************************************************************\
* PURPOSE:  minor review program to check the procs.u file.
*
*	Reads the "*.c" module and generates list of all routines.
*
*
\***************************************************************************/
#include <stdio.h>
#define MAX 300
#define MAXSTR 300
int uscanc (char *in, char *out[], int *nwrd, char *limit1, char *limit2);

main()
{
  int not_found;
  char str[MAXSTR];
  char rec[MAXSTR][MAX];
  char *hold;
  int i,n,j, nwrd, numn, nman;
  char *words[20];
  char *limit1 = ",=", *limit2 = " \t\n";

  FILE *fptr;

  if(( fptr = fopen("gui.c", "r" )) == 0 )
  {
     printf("\n ================================================\n");
     printf(  " =** UNABLE TO OPEN GUI.C FILE              **=\n");
     printf(  " ================================================\n\n");
  }
 
  n = 0;
  
  while ((fgets( str, 132, fptr )) != NULL)
  {
    n++; 

    if( strncmp( str, "main", 4) == 0 )
    {
      printf("%d %s\n", n, str );
      while ((fgets( str, 132, fptr )) != NULL && str[0]!= '{' )
      {
	n++; 
	printf("%s\n", str );
      }

      printf("Begin MAIN at rec %d\n", n );
      while ((fgets( str, 132, fptr )) != NULL && str[0]!= '}' )
      {
	n++; 
        /* printf("main: %s\n", str ); */
      }
      printf("End MAIN at rec %d\n", n );
      break;	/* break out of while loop - begin next part */
   }
  }
 

printf("Begin routine search\n");

  i = 0;

  while ( fgets( str, 132, fptr )) 
  {
    if( ++i >= MAX) i=0;
    strcpy( &rec[0][i], str );
    printf("%d %s\n", i, rec[0][i] );
    if( rec[0][i] == '{'  )
    {
      printf("WE have a routine!\n" );
      not_found = 1;
      j = i;

	/* back up to find the name */
      while( not_found )
      {
	j--;
	if( j<0 ) j = MAX;
	printf("?? %d %s\n", j, rec[j] );
	/* backup to find the name */
	if( *rec[j] == '\0' )
 	{
	  printf("NOT FOUND!\n");
	  break;
	}

	if( strncmp( rec[j], "voi", 3 ) == 0 ||
	    strncmp( rec[j], "dou", 3 ) == 0 ||
	    strncmp( rec[j], "sta", 3 ) == 0 ||
	    strncmp( rec[j], "Wid", 3 ) == 0 ||
	    strncmp( rec[j], "int", 3 ) == 0 ||
	    strncmp( rec[j], "flo", 3 ) == 0 ||
	    strncmp( rec[j], "lon", 3 ) == 0 )
	{
	   not_found = 0;
	   printf("ROUTINE: %s\n", rec[j] );
	}
      }
    }
  }

/*
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
*/
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

