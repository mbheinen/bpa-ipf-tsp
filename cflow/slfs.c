/* slfs.c
  This CFLOW procedure looks up flows and creates a report
  of flows between buses as listed in an input data file.
  Before the CFLOW procedure is called, a solved power flow case
  must be resident in the powerflow server.
  This CFLOW procedure prompts the user for two file names:
  the output file name to put the line flow listing in,
  the input file name of a file to get line data from.
  The input file has a LIN card for each branch to be monitored.
  If there are multiple lines between the same buses, slfs.c
  picks up all lines. A TOT card flags printing of total flow
 since the last TOT card (or since the beginning). Input lines
 without either LIN or TOT are printed directly to the output file.
 Each data card has LIN in columns 1-3 and the Branch identifiers
 in columns 7 to 31. First Bus name and KV in columns 7-18,
 Second bus name and KV in columns 20-31. Column 5 is a flag to
 tell slfs.c whether to use the "Pin" or "Pout" data quantity for
 the total flow. If the flag is "2", then "Pout" is used; otherwise
 "Pin" is used. The flag corresponds to the metering point.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cflowlib.h"
/* cf_debug = 1; /* Put this where you want the buffers displayed,for debugging */
int readln( char *s, int lim ) /* Read a line from the terminal input */
{
    int i;
    char c;
    for ( i=0; i < lim - 1 && ( c = getchar() ) != EOF && c != '\n'; ++i )
        s[i] = c;
    s[i] = '\0';
    return i;
}
int freadln( FILE *fp, char *s, int lim ) /* Read a line from a file */
{
    int i;
    char c;
    for ( i=0; i < lim - 1 && ( c = getc( fp ) ) != EOF && c != '\n'; ++i )
        s[i] = c;
    s[i] = '\0';
    return i;
}
main ( int argc, char *argv[] )
{
    pf_comments comments; /* CFLOW structure */
    pf_rec r; /* CFLOW structure */
    FILE *out, *dat;
    char card[82], cout[31];
    char skv1[5], skv2[5];
    char direc;
    int error, lin, tot, head;
    float brflow, totalflow = 0.0;
    float kv1, kv2;
    pf_cflow_init( argc, argv );
    /* Open the input file. This could be prompted for, but Don says
     he always uses the same input file, which will NOT be in the
     execution directory,so full pathname is supplied.
     If the file cannot be opened, the program terminates
     */
    dat=fopen( "/home/ipf/cflow_progs/slfs.dat", "r" );
    if ( dat == NULL ) {
        printf("Can't open data file\n");
        exit(0);
    }
    /* Prompt the user for the output file name, and open the file.
    Normally, file will be created in the execution directory,
    so cout is only 30 characters.
    If the file cannot be opened, the program terminates.
    */
    printf("Enter output file name > ");
    readln( cout, sizeof(cout) );
    printf("\n");
    out=fopen( cout, "w" );
    if ( out == NULL ) {
        printf("Can't open output file\n");
        exit(0);
    }
    /* Retrieve caseid and description, and print heading */
    error = pf_rec_comments( &comments, "G" );
    fprintf(out,"\n%s\n\n", &comments.h[0][33] );
    totalflow = 0.0;
    /* Loop for every line in the data file
    Read a line from the data file, if not "LIN" or "TOT" then
    print the text as is.
    If "LIN" retrieve and print line data.
    If "TOT" print totalflow and reinitialize it.
    */
    /* "C" array indices start at zero; below we use explicit starting index
    of 1 for "card" character array (string), so that column numbers
    will match array indices.
    */
    while ( freadln( dat, &card[1], sizeof( card ) - 1 ) )
    {
        lin = (!( strncmp( &card[1], "LIN", 3 )) );
        tot = (!( strncmp( &card[1], "TOT", 3 )) );
        head = lin + tot; /* = 0 if not either */
        if (head == 0) /* Heading card - print the text */
        {
            fprintf(out,"%s\n",&card[1]); /* Print from col. 1 - 1st char. is NULL! */
        } /* end if Heading */
        if (lin) /* LIN data card - process line */
        {
            /* Using CFLOW pf_init_branch function; bus kv’s must be real numbers */
            strncpy(skv1, &card[15], 4 );
            skv1[4] = '\0';
            strncpy(skv2, &card[28], 4 );
            skv2[4] = '\0';
            kv1 = atof(skv1);
            kv2 = atof(skv2);
            
            /* pf_init_branch stores the passed ID fields in r, and zeroes all the other fields */
            pf_init_branch ( &r, "L", &card[7], kv1, &card[20],kv2, "*", 0);

            /* Call CFLOW pf_rec_branch to retrieve output solution data */
            error = pf_rec_branch( &r, "O" );
            /* If branch not found, do not print anything. This happens quite often, since
            the input file is canned, and used on all cases. */
            if ( error ) {
                continue;    /* This sends it back to the ’while’ */
            }
            /* Otherwise, retrieve flows. r is defined in this pgm as a structure of type pf_rec
            i indicates input data, s indicates output data. See cflowlib.h for definitions.
            */
            direc = card[5]; /* Meter flag for Pin or Pout */
            if ( direc == '2' )
            {   /* meter at second bus */
                brflow = r.s.branch.Pout;
                totalflow += brflow;
                fprintf( out,"%s - %s(M) %s kV num ckts %d %7.1f\n",
                         r.i.branch.bus1_name, r.i.branch.bus2_name, skv2,
                         r.s.branch.num_ckt, brflow );
            }
            else /* meter at first bus */
            {
                brflow = r.s.branch.Pin;
                totalflow += brflow;
                fprintf( out,"%s(M) - %s %s kV num ckts %d %7.1f \n",
                         r.i.branch.bus1_name, r.i.branch.bus2_name, skv2,
                         r.s.branch.num_ckt, brflow );
            }
        } /* end if LIN */
        if (tot) /* TOT data card */
        {
            fprintf(out,
                    "\n Total flow is %7.1f \n\n",totalflow);
            totalflow = 0.0 ;
        } /* end else if TOT */
    } /* end while */
    pf_cflow_exit();
} /* end main */