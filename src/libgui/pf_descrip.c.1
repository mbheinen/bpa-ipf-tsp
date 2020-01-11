static char sccsid[]="@(#)pf_descrip.c	20.9 4/10/96";
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
#include <string.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include "ipf_ipc.h"            /* from ipc library - defines BUFSIZE,
                                                ENFOFMSG and SEPCHARS */
#define	 MAX_CMTS	20
#define	 MAX_CMTS_LEN	132
#define	 MAX_CMTS_SIZE	MAX_CMTS * MAX_CMTS_LEN 
#define  MAX_HDR_LEN	120

static char outbuf[BUFSIZE];         /* contains messages to powerflow (8192 chars)*/
static char inbuf [BUFSIZE];         /* contains messages from powerflow */

static char buf[MAX_CMTS_SIZE];
 
extern int ipc_synch_rw			( char *, char * );
extern void lookup_and_fill_field	( char *, char *, int, int );
extern void lookup_and_get_field	( char *, char *, int );
extern void strcat_field		( char *, char *, int, char *fmt );
extern int  reformat			( char *, char *, char * );
extern Widget widget_id			( char * );
extern void add_blanks_to_string	( char *, int );
extern lookup_and_reduce_text_field	( char *text_name, int size );

void add_line_to_pf_string		( char *start_str, char *wdgt_name,
					  int len, char *fmt );
void add_line_to_pf_str_w		( char *key_str, char *string,
					  int len, char *fmt );

#define MAX_COM 20
void fill_descrip_form ( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/**********************************************************************\
*
*
*
\**********************************************************************/
{
  char *psubstr;                /* ptr to sub-string - used by strtok to cut
                                   up the inbuf string */
  Widget id;
  char comstr[MAX_COM*256];
  int hdr_num, com_num;

  hdr_num = 0;
  com_num = 0;
  memset(comstr, '\0', sizeof(comstr));

  sprintf( outbuf, "/get_data,type=COMMENTS\n%s\n", ENDOFMSG );

  ipc_synch_rw( inbuf, outbuf);         /* transmit inbuf to powerflow, */
 
  psubstr = strtok( inbuf, SEPCHARS );

  /* now get all the lines of data */
  while( psubstr != NULL )
  {
     /* check for the logical end of the message */
    if( !strncmp( psubstr, ENDOFMSG, sizeof(ENDOFMSG)) )
        break;

    if ( !strncmp ("CASE_ID", psubstr, 7 ) )
    {
      lookup_and_fill_field( "pf_case_id_text", psubstr, 10, 19 );
      lookup_and_reduce_text_field( "pf_case_id_text", 10 );
    }

    else if( !strncmp ("CASE_DS", psubstr, 7 ) )
    {
      lookup_and_fill_field( "pf_case_descrip_text", psubstr, 10, 29 );
      lookup_and_reduce_text_field( "pf_case_descrip_text", 20 );
    }

    else if( !strncmp ("H", psubstr, 1 ) )
    {
      hdr_num++;
      switch (hdr_num)
      {
        case 1:
          lookup_and_fill_field( "pf_header_1_text", psubstr, 1, MAX_HDR_LEN );
        break;

        case 2:
          lookup_and_fill_field( "pf_header_2_text", psubstr, 1, MAX_HDR_LEN );
        break;

        case 3:
          lookup_and_fill_field( "pf_header_3_text", psubstr, 1, MAX_HDR_LEN );
        break;
      }
    }

    else if( !strncmp ("C", psubstr, 1 ) )
    {
      if(com_num < MAX_COM) {
        strcat(comstr, psubstr+1);
        strcat(comstr, "\n");
        com_num++;
      }
    }
    else if( !strncmp ("/", psubstr, 1 ) )
    {
	 /* get rid of command and status returned  */
    }
    else
    {
      printf("*** INFO *** From routine fill_descrip_form (module pf_descrip.c)\n" );
      printf("             Unknown string type in COMMENTS from PF\n");
      printf("(%s)\n", psubstr );
    }

    psubstr = strtok( NULL, SEPCHARS );
  }

  if(com_num == 0) {
    comstr[0] = ' ';
  }
  id = widget_id( "pf_comments_text" );
  if(id) XmTextSetString(id,comstr);

#ifdef JUNK
  id = widget_id( "pf_comments_text" );
  if( id ) XtVaSetValues( id, XmNvalue, comment_rec, NULL );
    id = widget_id( "pf_comments_text" );
    if( id ) XtVaSetValues( id, XmNlistItems, comment_rec,
				XmNlistItemCount, com_num, NULL );
#endif
  
}


void send_descrip_to_pf( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs)
/*************************************************************************\
* Purpose: retrieve all data from pf_descrption form and build a comments
*	   record to send to powerflow.
*
* Called by: "Apply pushbutton on 
*
* Author: Bill Rogers  11/93
\*************************************************************************/
{
  int ipf_status, num_cmts=0, len;
  char *end_mark, *start, *pnew_txt_widget_str;
  Widget id;

  sprintf( outbuf, "/put_data,type=COMMENTS\n" );

  add_line_to_pf_str_w ("CASE_ID = ", "pf_case_id_text",           11, "a10"  );
  add_line_to_pf_str_w ("CASE_DS = ", "pf_case_descrip_text",      21, "a20"  );
  add_line_to_pf_string("H",          "pf_header_2_text", MAX_HDR_LEN, "a130" );
  add_line_to_pf_string("H",          "pf_header_3_text", MAX_HDR_LEN, "a130" );

/*  lookup_and_get_field( "pf_comments_text", buf , MAX_CMTS_SIZE ); */
  if( ( id = widget_id( "pf_comments_text" ) ) )
  {
    pnew_txt_widget_str = XmTextGetString(id);

   /* could overwrite memory if shortage of space in calling program */
    strcat(buf, pnew_txt_widget_str );
    XtFree(pnew_txt_widget_str);
  }

  start = buf;			 /* start addres of each record */
  end_mark = strchr( buf, '\n' );/* end address of each record	*/
  num_cmts=0;

  if( end_mark==0 && strlen(start) )	 /* in case of no end-mark then */
	 end_mark = buf + strlen(start); /* compute address from length */

  while( end_mark )
  {
    if( num_cmts>=MAX_CMTS )
    {  printf( "*** WARNING *** Number of PF comments limited to %d lines.\n             Remaining comments ignored.\n", MAX_CMTS );
      break;
    }

    num_cmts++;
    strcat( outbuf, "C" );
    len = end_mark - start + 1;	/* compute length from start and end addr */
    if( len > (MAX_CMTS_LEN-1) ) len = MAX_CMTS_LEN-1; /* clip */

    strncat( outbuf, start, len );	/* copy the record */

    start = ++end_mark;
    end_mark = strchr( start, '\n' );	/* add a CR marker */
    if( end_mark==0 && strlen(start) ) end_mark = start + strlen(start);

  } 

  len = strlen( outbuf );	/* powerflow can't tolerate *[EOM] at end of */
				/* record. *[EOM] must go on line by itself  */
  if( outbuf[len-1] != '\n' ) strcat( outbuf, "\n" );

  strcat( outbuf, ENDOFMSG );

/*printf("Generated send string is:\n%s\n", outbuf );*/

  ipf_status = ipc_synch_rw( inbuf, outbuf); /* transmit inbuf to powerflow, */

/*printf("STATUS:%d\n", ipf_status );*/
}


void add_line_to_pf_string( char *key_str, char *text_widget_name, int len,
			    char *fmt )
/************************************************************************\
* Purpose: as key word and widget text string IF text string exists.
*
*
\************************************************************************/
{

  lookup_and_get_field( text_widget_name, buf, len );
  if( strlen(buf) )
  {
    strcat( outbuf, key_str );
    strcat_field( outbuf, text_widget_name, len, fmt );
    strcat( outbuf, "\n" );
  }

}

void add_line_to_pf_str_w ( char *key_str, char *text_widget_name, int len,
			    char *fmt )
/************************************************************************\
* Purpose: same as above, except the blanks must be '#'
*
*
\************************************************************************/
{
  int i;
  char text_str[40];

  lookup_and_get_field( text_widget_name, text_str, len );

  for( i=0 ; i<=(int)strlen(text_str) ; i++ )
  {
    if ( text_str[i]==' ' ) text_str[i]='#';
  }

  if( strlen(text_str) )
  {
    strcat( outbuf, key_str );
    reformat( text_str, text_str, fmt );
    add_blanks_to_string( text_str, len );
    strcat( outbuf, text_str );
    strcat( outbuf, "\n" );
  }

}
