static char sccsid[]="@(#)ipfdefine.c	1.16 8/1/95";
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
#include <Xm/Xm.h>
#include <Xm/SelectioB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include "dmgr.h"
#include "coord_data.h"
#include "ipf_ipc.h"

static char     outbuf_array[BUFSIZE];
static char     inbuf_array [BUFSIZE];
char *define_name = "define_selection_dialog";

void fillDefineList		();
int write_out_define_cards	( FILE *fptr );
int add_define_cards_to_str	(char *pf_send_rec );
void define_apply_cb		( Widget wid, XtPointer tag,
				  XmSelectionBoxCallbackStruct *cb );
void define_help_cb		( Widget wid, XtPointer tag,
				  XmSelectionBoxCallbackStruct *cb );
int selection_box_getwid	( Widget *sblstid, Widget *sbtxtid );


extern Widget widget_id		( char *name); 
extern void fetch_widget   	( char *wname );
extern void send_comments_and_defines_to_pf( );
extern int  HashLookup		( char *name, Widget *id);
extern void clip_outside_blanks ( char *string );

#define MAXDEFINES	100



void fillDefineList()
/*****************************************************************************\
* Purpose: read the coord db and extract all the DEFINE records and
*		put them into the MOTIF list widget.
*
* Author:  Bill Rogers		March 1, 1995
*
* Called by: apply_files (filedlgrtn.c)
\*****************************************************************************/
{
  PSCoordFileRecord defn_rec;         /* storage for db_search key values    */
  PSCoordFileRecord *pdefn_rec;        /* ptr to key for bus search           */
  PSCoordFileRecord *nextrec;          /* ptr assigned to address of next rec */
  int num=0;

  XmString define_rec[MAXDEFINES];
  Widget id, list_id, text_id;

  DB_STAT stat;

  if( db_valid_db(&pscor_db ) != D_OKAY ) return;

  fetch_widget ( define_name );

  if(( id = widget_id( define_name )) == FALSE ) return;

  if(selection_box_getwid( &list_id, &text_id) == False)  
	return;

   /* additional editing features may be added in at a later time */
  XtSetSensitive( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_OK_BUTTON ),
								 False );
#ifdef notdef
  XtSetSensitive( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_HELP_BUTTON ),
								 False );
  XtSetSensitive( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_APPLY_BUTTON ),
								 False );
#endif

  pdefn_rec = &defn_rec;
  pdefn_rec->cor.idx = 0;
  strncpy(pdefn_rec->cor.key, PSKEY_DEFINES,
                         sizeof(pdefn_rec->cor.key ) );

  stat = db_search( &pscor_db, &pdefn_rec, &nextrec, COORD_KEY_IDX );
  if(stat == D_EOF)
  {
    return;		 /* no records for this type found so return */
  }

  if(stat == D_NOTFOUND)
  {
      /* no record found with key == defines AND idx == 0 */
      /* so now see if some record was returned anyway */
    if(strncmp(nextrec->cor.key, PSKEY_DEFINES, 1 ) == 0)
    {
       stat = D_FOUND;
    }

    while(stat == D_FOUND && num<MAXDEFINES )
    {
      define_rec[num++] = XmStringCreate( (char *)nextrec->cor.key,
                                XmSTRING_DEFAULT_CHARSET );

      pdefn_rec = nextrec;
      stat = db_next( &pscor_db, &pdefn_rec, &nextrec, COORD_KEY_IDX );
      if(strncmp(nextrec->cor.key, PSKEY_DEFINES, 1 ) != 0)
      {
         stat = D_NOTFOUND;
      }

    }
  }


	  /* ensure list box is empty */
  XmListDeleteAllItems( list_id );

	  /* dump the defines into MOTIF list box */
  XtVaSetValues( id, XmNlistItems, define_rec, XmNlistItemCount, num, NULL );


}




int write_out_define_cards	( FILE *fptr )
/****************************************************************************\
* Purpose: Gets the total MOTIF list of define cards and writes them
*		to fptr.
*
* Called by: pswriteCoordFile (pscordat.c)
*	     
\****************************************************************************/
{
  char  *buf_array = outbuf_array;
  int num;

  buf_array[0]= '\0';

  num = add_define_cards_to_str ( buf_array );

  fprintf( fptr, "%s", buf_array );

  return ( num );
}

int add_define_cards_to_str (char *str )
/****************************************************************************\
* Purpose: Gets the total MOTIF list of define cards and adds them
*		to string.
*
* Called by: write_out_define_cards (above)
*	     send_comments_and_defines_to_pf (graphpscor.c)
\****************************************************************************/
{
  Widget list_id, text_id;
  XmString *strlist;
  char	   *text;
  int i, num_items, rtncde;

     /* NOTE: use HashLookup because widget_id gives unwanted warnings */
  if( HashLookup( define_name, &list_id ) == False )
        return;		/* Never opened or filled, thus return w/o action */
 
  if(selection_box_getwid( &list_id, &text_id) == False)  
	return;

  XtVaGetValues( list_id, 
	 XmNitemCount, &num_items, XmNitems, &strlist, NULL);

  for(i = 0; i < num_items; i++)
  {
    XmStringGetLtoR( strlist[i], XmSTRING_DEFAULT_CHARSET, &text);
    clip_outside_blanks( text );
    strcat( str, text );
    strcat( str, "\n" );
    XtFree ( text );
  }

  return ( num_items );
}


void define_apply_cb(Widget wid,XtPointer tag,XmSelectionBoxCallbackStruct *cb)
/****************************************************************************\
* Purpose:  answers the APPLY pushbutton.  Takes editable field and
*	    replaces it into the LIST box.
*
\****************************************************************************/
{
  Widget list_id;
  Widget text_id;
  char *stmp;
  int *position_list;
  int position_count;

  if(selection_box_getwid( &list_id, &text_id) == False)  
	return;

  XmTextShowPosition  ( text_id,(XmTextPosition)0);
  XmListGetSelectedPos( list_id, &position_list, &position_count);

  if(position_count <= 0) {
	printf("define_apply: info - no postion selected\n");
	return;
  }

  XmListReplaceItemsPos(list_id, &cb->value, (int)1, position_list[0]);

  return;
}


void define_help_cb(Widget wid,XtPointer tag,XmSelectionBoxCallbackStruct *cb)
/**************************************************************************\
* Purpose:  Answers the HELP (Create New) pushbutton.
*
* Called by:
\**************************************************************************/
{
  Widget list_id;
  Widget text_id;
  char *stmp;
  int *position_list;
  int position_count;
  XmString xstr;
  char *str;

  if(selection_box_getwid( &list_id, &text_id) == False)
	return;

  XmTextShowPosition(text_id,(XmTextPosition)0);
  XmListGetSelectedPos(list_id, &position_list, &position_count);
  if(position_count <= 0) {
	printf("define_apply: info - no postion selected\n");
	return;
  }

  str = XmTextGetString(text_id);
  xstr = XmStringCreateLtoR(str, XmFONTLIST_DEFAULT_TAG );
  XmListAddItems(list_id, &xstr, (int)1, position_list[0]);
  XmStringFree(xstr);

  return;
}




void define_cancel_cb(Widget wid, XtPointer tag, XtPointer *cbd)
/**************************************************************************\
* Purpose:  Answers the CANCEL (close) pushbutton.
\**************************************************************************/
{
  /* printf("define_cancel_cb\n"); */
  send_comments_and_defines_to_pf();
}




void define_ok_cb(Widget wid, XtPointer tag, XtPointer *cbd)
/**************************************************************************\
* Purpose:  Answers the OK (Edit) pushbutton.
\**************************************************************************/
{
  printf("define_ok_cb\n");
}



int selection_box_getwid( Widget *sblstid, Widget *sbtxtid)
/**************************************************************************\
* Purpose:  Get the wid_id of the selection box XmDIALOG_LIST and
*		XmDIALOG_TEXT		
*
* Called by:
\**************************************************************************/
{
  Widget sbid;

  if(( sbid = widget_id(define_name)) == FALSE) 
	return(False);

  *sblstid = (Widget)XmSelectionBoxGetChild( sbid, XmDIALOG_LIST);
  if(*sblstid == NULL)
  {
    printf("PROGRAMMER ERROR no XmDIALOG_LIST for %s\n", define_name);
		return(False);
  }

  *sbtxtid = (Widget)XmSelectionBoxGetChild( sbid, XmDIALOG_TEXT);
  if( *sbtxtid == NULL)
  {
    printf("PROGRAMMER ERROR no XmDIALOG_TEXT for %s\n", define_name);
    return(False);
  }
  return(True);
}
