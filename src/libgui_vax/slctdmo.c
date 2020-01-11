static char sccsid[]="@(#)slctdmo.c	20.2 5/16/94";
/*****************************************************************************\
*	Function: 
*		
*
\*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include "dmgr.h"
#include "base_data.h"

void lookup_and_get_field();
void change_field();

extern int area_box_fill_flag;
extern int branch_box_fill_flag;
extern int bus_box_fill_flag;
char org_rec[100];
char new_rec[100];
char bus_type;

Widget list_widget;
XmListCallbackStruct *cbs;





/*****************************************************************************\
*       Function: lookup_and_fill_field.c
*
*       Author: B Rogers      Modified: Mar 17, 1992
*
*       Called by:
*
\*****************************************************************************/

void lookup_and_fill_field( fld_name, string, col_1, col_2 )
{
  Widget id;
  char ret_string[80];
  int j=0, lbk=0, len;

  if((HashLookup( fld_name, &id)) == FALSE)
  {
    printf("\nCan't find %s id\n", fld_name );
  }
  stringPart( string, ret_string, col_1, col_2 );

/*  step through string to find position of last non-blank character */
  len = col_2 - col_1 + 1;
  while ( ret_string[j] != '\n' && j<len )
  {
     if ( ret_string[j++] != ' ' )  lbk = j;
  }

  ret_string[lbk] = 0;  /* insert end-of-string marker */

  XmTextSetString( id, ret_string );

}



/*****************************************************************************\
*	Function: fill_area_selection_box.c 
*		
*	Author: Dan Clark/B Rogers	Modified: Feb 19, 1992
*
*	Called by: 
*
\*****************************************************************************/

int fill_area_selection_box ( widget, tag, callback_data)
Widget		widget;
caddr_t		*tag;
caddr_t		*callback_data;


{

  Widget		id, list_id;
  int i=1;

  PFBASERECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */
  PFBASERECORD *branchvalrec;/* ptr to key for branch search */
  PFBASERECORD *nextbranchrec;/* ptr returned for next branch rec */
  PFBASERECORD *busvalrec;	/* ptr to key for bus search */
  PFBASERECORD *nextbusrec;	/* ptr returned for next bus rec */

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statbus;	/* status returned for bus queries of db */
  DB_STAT statbranch;	/* status returned for branch queries of db */

  if ( area_box_fill_flag == FALSE )
  {

    if ( ( HashLookup("area_selection_dialog", &id ) ) == FALSE )
       printf("\nCan't find area_selection_dialog id\n");

    list_id = (Widget)XmSelectionBoxGetChild( id, XmDIALOG_LIST);
 
    XmListDeleteAllItems( list_id );

    /** retrieve all the records (in key order) and write to output file **/
    stat = db_first(&base_db, &nextrec, BSE_TYPE);
    if (stat != D_FOUND)
    {
      printf("ERROR db_search did not find any records \n");
      return(1);
    }

    busvalrec = &keyvalrecord;
    strncpy(busvalrec->type, "A", 1 );
    strncpy(busvalrec->branch.name1, "            ", 12 );
    stat = db_search( &base_db, &busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

    if ( stat != D_FOUND )
    {
       if (stat == D_EOF) {
         printf("Warning: end of table reached\n");
       } else {
         stat = D_FOUND;
       }
    }
   	
    while(stat == D_FOUND)
    {
      if( nextbusrec->type[0] != 'A')
          break;
      /*printf("%s", nextbusrec); */

      XmListAddItem( list_id,
		   XmStringCreate( nextbusrec, XmSTRING_DEFAULT_CHARSET ),	
		   i++ );

      busvalrec = nextbusrec;     /* set up next index */

      stat=db_next( &base_db, &busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

    }

    printf("%d items added into area selection box\n", i );
    area_box_fill_flag = TRUE;
  }

  return(0);

}

/*****************************************************************************\
*	Function: build_bus_record
*
*	Author: Bill Rogers
*		Mar 18, 1991		Modified:
*
*	Called by: OK push button on bus dialog box.
*
\*****************************************************************************/
void build_bus_record( w, tag, callback_data )
Widget w;
caddr_t *tag;
caddr_t *callback_data;

{
  char bus_record[100];
  char bus_namx[9];
  char bus_volt[5];
  char bus_ownr[4];
  char bus_zone[3];
  int	**position, *count, pos;
  Widget	id, text_id, list_id;

  PFBASERECORD keyvalrecord;    /* storage for db_search key values */
                                /* used for any search */
  PFBASERECORD *keyvalrec;      /* ptr to key values in db_search  */
                                /* or ptr to current record in db_next */
                                /* similar to a cursor in sql */
  PFBASERECORD *nextrec;        /* ptr assigned to address of next rec */
                                /* used by db_search & db_next */
  PFBASERECORD *branchvalrec;/* ptr to key for branch search */
  PFBASERECORD *nextbranchrec;/* ptr returned for next branch rec */
  PFBASERECORD *busvalrec;      /* ptr to key for bus search */
  PFBASERECORD *nextbusrec;     /* ptr returned for next bus rec */

  DB_STAT stat;         /* status returned for general db access */
  DB_STAT statbus;      /* status returned for bus queries of db */
  DB_STAT statbranch;   /* status returned for branch queries of db */
 
    new_rec[0] = 'B';
    new_rec[1] = bus_type;
    new_rec[2] = 'M';
    new_rec[3] = 0;

    lookup_and_get_field( "bus_elec_dia_owner_text",     bus_ownr );
    lookup_and_get_field( "bus_elec_dia_name_text",      bus_namx );
    lookup_and_get_field( "bus_elec_dia_kv_text",        bus_volt );
    lookup_and_get_field( "bus_elec_dia_zone_text",      bus_zone );

    strcat( new_rec, bus_ownr );
    strcat( new_rec, "        " );
    new_rec[6] = 0;
    strcat( new_rec, bus_namx );
    strcat( new_rec, "        " );
    new_rec[14] = 0;
    strcat( new_rec, bus_volt );
    strcat( new_rec, "        " );
    new_rec[18] = 0;
    strcat( new_rec, bus_zone );
    strcat( new_rec, "        " );
    new_rec[20] = 0;
 
    change_field( org_rec, new_rec, "bus_elec_dia_load_p_text", 20, 24, 1 );
    change_field( org_rec, new_rec, "bus_elec_dia_load_q_text", 25, 29, 1 );
    change_field( org_rec, new_rec, "bus_elec_dia_shunt_p_text",30, 33, 1 );
    change_field( org_rec, new_rec, "bus_elec_dia_shunt_q_text",34, 37, 1 );

    switch (bus_type)
    {
      case ' ':
      case 'C':
      case 'V':
       change_field( org_rec, new_rec, "bus_elec_dia_max_p_text",  38, 41, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_gen_p_text",  42, 46, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_sched_q_text",47, 51, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_min_q_text",  52, 56, 1);
      break;

      case 'E':
      case 'G':
      case 'Q':
      case 'X':
       change_field( org_rec, new_rec, "bus_elec_dia_max_p_text2", 38, 41, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_gen_p_text2", 42, 46, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_max_q_text",  47, 51, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_min_q_text2", 52, 56, 1);
      break;

      case 'S':
       change_field( org_rec, new_rec, "bus_elec_dia_max_p_text",  38, 41, 1);
       strcat      ( new_rec, "     " );
       change_field( org_rec, new_rec, "bus_elec_dia_sched_q_text",47, 51, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_min_q_text",  52, 56, 1);
      break;
    }

    switch (bus_type)
    {
      case 'S':
      case 'E':
      case 'Q':
       change_field( org_rec, new_rec , "bus_elec_dia_hold_v_text", 57, 60, 4);
      break;

      case 'X':
      case 'G':
       change_field( org_rec, new_rec, "bus_elec_dia_v_max_text", 57, 60, 4);
       change_field( org_rec, new_rec, "bus_elec_dia_v_min_text", 61, 64, 1);
       change_field( org_rec, new_rec, "bus_elec_dia_remote_name_text",65,72,1);
       change_field( org_rec, new_rec, "bus_elec_dia_remote_kv_text",  73,76,1);
       change_field( org_rec, new_rec, "bus_elec_dia_remote_pcs_text", 77,79,1);
      break;
    }

    switch (bus_type)
    {
      case 'S':
       change_field( org_rec, new_rec, "bus_elec_dia_phase_text",  61, 63, 1);
      break;
    }

    printf("New modify record is:\n(%s)\n", new_rec );
    pfchanges_cb(0,new_rec,0);

/** Update Selection Box selection text. JLR - 3 Apr **/

    if((HashLookup("bus_selection_dialog", &id)) == FALSE)
    {
      printf("\nWarning - Can't find bus_selection_dialog id\n");
    } else {
      text_id = XmSelectionBoxGetChild(id, XmDIALOG_TEXT);
      list_id = XmSelectionBoxGetChild(id, XmDIALOG_LIST);

      XmTextSetString(text_id, new_rec);
    }

/* Doesn't work
    XmListGetSelectedPos(list_id, &position, &count);
    pos = (int)*position;
    XtFree(position);
    XmListReplaceItemsPos(list_id,
                          XmStringCreate( new_rec, XmSTRING_DEFAULT_CHARSET ),
                          1,
                          pos);
*/
/** JLR ***/ 

/*    XmListReplaceItemsPos( list_widget,
*		   XmStringCreate( new_rec, XmSTRING_DEFAULT_CHARSET ),
*                           1, 
*			   cbs->item_position );
*
*    db_search( &base_db, searchrec,  retrec,  fieldnum );
*    db_update( &base_db, searchrec,  new_rec, );
*
*    VUIT_Unmanage("bus_elec_info_dialog");
*/
}


/*****************************************************************************\
*	Function: fill_branch_dialog
*
*		  After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*
*	Author: John Rutis - from fill_bus_dialog by Bill Rogers
*		Feb 25, 1991		Modified:
*
\*****************************************************************************/
 
void fill_branch_dialog(char *value)
{
  Widget id;   

  char ret_string[80];

  if((HashLookup("bal_line_info_bus_1_text", &id)) == FALSE)
  {
    printf("\nCan't find bus_elec_dia_name_text id\n");
  }

  stringPart(value, ret_string, 6, 13);
  XmTextSetString(id, ret_string); 


}


/*****************************************************************************\
*	Function: fill_branch_selection_box.c 
*		
*	Author: Dan Clark/Bill Rogers	Dec, 1991	Modified: Feb 19, 1992
*
*	Called by: "Branch" push button callback (edit_menu_branch_button)
*
\*****************************************************************************/

int fill_branch_selection_box ( widget, tag, callback_data)
Widget		widget;
caddr_t		*tag;
caddr_t		*callback_data;


{

  Widget		id, list_id;
  int i=0;
  XmString branch_record[10000];

  PFBASERECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */
  PFBASERECORD *branchvalrec;/* ptr to key for branch search */
  PFBASERECORD *nextbranchrec;/* ptr returned for next branch rec */
  PFBASERECORD *busvalrec;	/* ptr to key for bus search */
  PFBASERECORD *nextbusrec;	/* ptr returned for next bus rec */

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statbus;	/* status returned for bus queries of db */
  DB_STAT statbranch;	/* status returned for branch queries of db */

  if ( branch_box_fill_flag == FALSE )
  {

    if ( ( HashLookup("branch_selection_dialog", &id ) ) == FALSE )
       printf("\nCan't find branch_selection_dialog id\n");

    list_id = (Widget)XmSelectionBoxGetChild( id, XmDIALOG_LIST);
 
    XtSetSensitive ( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_HELP_BUTTON),
                      False );

    XtSetSensitive ( XmSelectionBoxGetChild( id, XmDIALOG_APPLY_BUTTON),
		     False );

    XmListDeleteAllItems( list_id );

    /** retrieve all the records (in key order) and write to output file **/
    stat = db_first(&base_db, &nextrec, BSE_TYPE);
    if (stat != D_FOUND)
    {
      printf("ERROR db_search did not find any records \n");
      return(1);
    }

    busvalrec = &keyvalrecord;
    strncpy(busvalrec->type, "L", 1 );
    strncpy(busvalrec->branch.name1, "            ", 12 );
    stat = db_search( &base_db, &busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

    if ( stat != D_FOUND )
    {
       if(stat == D_EOF) {
         printf("Warning: End of table reached\n");
       } else {
         stat = D_FOUND;
       }
    }
   	
    while(stat == D_FOUND)
    {
      if( nextbusrec->type[0] != 'L' && nextbusrec->type[0] != 'T' &&
	  nextbusrec->type[0] != 'E' && nextbusrec->type[0] != 'R' )
        break;
      /*printf("%s", nextbusrec); */

      branch_record[i] = XmStringCreate( nextbusrec, XmSTRING_DEFAULT_CHARSET);
      i++;

/*      XmListAddItem( list_id,
		   XmStringCreate( nextbusrec, XmSTRING_DEFAULT_CHARSET ),	
		   i++ );
*/
      busvalrec = nextbusrec;     /* set up next index */

      stat=db_next( &base_db, &busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

    }

    XtVaSetValues( id, XmNlistItems, branch_record, XmNlistItemCount, i, NULL);

    printf("%d items added into selection box\n", i );
    branch_box_fill_flag = TRUE;
  }

  return(0);

}

/*****************************************************************************\
*	Function: fill_bus_out_texts
*
*		  Fills the text boxed in the bus output dialog
*
*	Author: J Rutis
*		Apr 7, 1992		Modified:
*
\*****************************************************************************/
 
void fill_bus_out_texts(bus_output_string)
char *bus_output_string;
{
  lookup_and_fill_field("bus_out_dia_owner_text",    bus_output_string,  3, 5);
  lookup_and_fill_field("bus_out_dia_name_text",    bus_output_string,  6, 13);
  lookup_and_fill_field("bus_out_dia_kv_text",    bus_output_string, 14,17);
  lookup_and_fill_field("bus_out_dia_zone_text",    bus_output_string, 18,19);
  lookup_and_fill_field("bus_out_dia_p_gen_text",    bus_output_string, 20,27);
  lookup_and_fill_field("bus_out_dia_q_gen_text",    bus_output_string, 28,35);
  lookup_and_fill_field("bus_out_dia_volts_text",    bus_output_string, 36,43);
  lookup_and_fill_field("bus_out_dia_angle_text",    bus_output_string, 44,51);
  lookup_and_fill_field("bus_out_dia_p_load_text",    bus_output_string, 52,59);
  lookup_and_fill_field("bus_out_dia_q_load_text",    bus_output_string, 60,67);
  lookup_and_fill_field("bus_out_dia_shunt_used_text",    bus_output_string, 68,75);
  lookup_and_fill_field("bus_out_dia_shunt_sched_text",    bus_output_string, 76,83);
}
/*****************************************************************************\
*	Function: fill_bus_out_dialog
*
*		  Fills the text boxed in the bus output dialog
*
*	Author: J Rutis
*		Apr 7, 1992		Modified:
*
\*****************************************************************************/
 
void fill_bus_out_dialog(w, tag, callback_data)
Widget		w;
caddr_t		*tag;
caddr_t		*callback_data;
{
  char *data = {"B. BPAFARGO ND200099111.1111222.2222333.3333444.4444555.5555666.6666777.7777888.8888"};

 /*  fill_bus_out_texts(data); */
}


/*****************************************************************************\
*	Function: fill_bus_dialog
*
*		  After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*
*	Author: B.Rogers
*		Feb 20, 1991		Modified:
*
\*****************************************************************************/
 
void fill_bus_dialog ( bus_string )
char *bus_string;

{
  Widget id;   
  Widget menu_id;   
  Widget button_id;   
  Widget option_button_id;   

  char ret_string[80];
  char bus_button[3];

  if((HashLookup("bus_elec_info_dialog", &id )) == FALSE)
  {
    printf("\nCan't find bus_elec_info_dialog id\n");
    return;
  }

  if((HashLookup("bus_dialog_type_menu", &menu_id )) == FALSE)
  {
    printf("\nCan't find bus_dialog_type_menu id\n");
    return;
  }

  option_button_id = (Widget)XmOptionButtonGadget(menu_id);

  strcpy( org_rec, bus_string );	/* for making BM card later.. */

  bus_type = bus_string[1];
  stringPart( bus_string, bus_button, 0, 1);

  switch(bus_type)
  {
    case ' ':
        if((HashLookup("bus_elec_dia_type_b_button", &button_id)) == FALSE)
        {
          printf("\nCan't find bus_elec_dia_type_b_button id\n");
          return;
        }

        XtVaSetValues(option_button_id, XmNlabelString,
           XmStringCreate("B ", XmSTRING_DEFAULT_CHARSET), NULL);

        VUIT_Manage("bus_info_gen_sched_bb");
        VUIT_Unmanage("bus_info_gen_max_bb");
        VUIT_Unmanage("bus_elec_dia_hold_v_text");
        VUIT_Unmanage("bus_elec_dia_v_hold_label");
        VUIT_Manage("bus_elec_dia_b_blank_pix_label");
        VUIT_Unmanage("bus_elec_dia_be_pix_label");
        VUIT_Unmanage("bus_elec_dia_bg_pix_label");
        VUIT_Unmanage("bus_elec_dia_bq_pix_label");
        VUIT_Unmanage("bus_elec_dia_bs_pix_label");
        VUIT_Unmanage("bus_elec_dia_bx_pix_label");
        VUIT_Unmanage("bus_elec_dia_remote_form");
        VUIT_Unmanage("bus_elec_dia_v_max_label");
        VUIT_Unmanage("bus_elec_dia_v_max_text");
        VUIT_Unmanage("bus_elec_dia_v_min_label");
        VUIT_Unmanage("bus_elec_dia_v_min_text");
        VUIT_Unmanage("bus_elec_dia_phase_label");
        VUIT_Unmanage("bus_elec_dia_phase_text");
    break;

    case 'E':
        if((HashLookup("bus_elec_dia_type_be_button", &button_id)) == FALSE)
        {
          printf("\nCan't find bus_elec_dia_type_be_button id\n");
          return;
        }

        XtVaSetValues(option_button_id, XmNlabelString,
           XmStringCreate("BE", XmSTRING_DEFAULT_CHARSET), NULL);

        VUIT_Unmanage("bus_info_gen_sched_bb");
        VUIT_Manage("bus_info_gen_max_bb");
        VUIT_Manage("bus_elec_dia_hold_v_text");
        VUIT_Manage("bus_elec_dia_v_hold_label");
        VUIT_Unmanage("bus_elec_dia_b_blank_pix_label");
        VUIT_Manage("bus_elec_dia_be_pix_label");
        VUIT_Unmanage("bus_elec_dia_bg_pix_label");
        VUIT_Unmanage("bus_elec_dia_bq_pix_label");
        VUIT_Unmanage("bus_elec_dia_bs_pix_label");
        VUIT_Unmanage("bus_elec_dia_bx_pix_label");
        VUIT_Unmanage("bus_elec_dia_remote_form");
        VUIT_Unmanage("bus_elec_dia_v_max_label");
        VUIT_Unmanage("bus_elec_dia_v_max_text");
        VUIT_Unmanage("bus_elec_dia_v_min_label");
        VUIT_Unmanage("bus_elec_dia_v_min_text");
        VUIT_Unmanage("bus_elec_dia_phase_label");
        VUIT_Unmanage("bus_elec_dia_phase_text");
    break;

    case 'G':
        if((HashLookup("bus_elec_dia_type_bg_button", &button_id)) == FALSE)
        {
          printf("\nCan't find bus_elec_dia_type_bg_button id\n");
          return;
        }

        XtVaSetValues(option_button_id, XmNlabelString,
           XmStringCreate("BG", XmSTRING_DEFAULT_CHARSET), NULL);

        VUIT_Unmanage("bus_info_gen_sched_bb");
        VUIT_Manage("bus_info_gen_max_bb");
        VUIT_Unmanage("bus_elec_dia_hold_v_text");
        VUIT_Unmanage("bus_elec_dia_v_hold_label");
        VUIT_Unmanage("bus_elec_dia_b_blank_pix_label");
        VUIT_Unmanage("bus_elec_dia_be_pix_label");
        VUIT_Manage("bus_elec_dia_bg_pix_label");
        VUIT_Unmanage("bus_elec_dia_bq_pix_label");
        VUIT_Unmanage("bus_elec_dia_bs_pix_label");
        VUIT_Unmanage("bus_elec_dia_bx_pix_label");
        VUIT_Manage("bus_elec_dia_remote_form");
        VUIT_Manage("bus_elec_dia_v_max_label");
        VUIT_Manage("bus_elec_dia_v_max_text");
        VUIT_Manage("bus_elec_dia_v_min_label");
        VUIT_Manage("bus_elec_dia_v_min_text");
        VUIT_Unmanage("bus_elec_dia_phase_label");
        VUIT_Unmanage("bus_elec_dia_phase_text");
    break;

    case 'Q':
        if((HashLookup("bus_elec_dia_type_bq_button", &button_id)) == FALSE)
        {
          printf("\nCan't find bus_elec_dia_type_bq_button id\n");
          return;
        }

        XtVaSetValues(option_button_id, XmNlabelString,
           XmStringCreate("BQ", XmSTRING_DEFAULT_CHARSET), NULL);

        VUIT_Unmanage("bus_info_gen_sched_bb");
        VUIT_Manage("bus_info_gen_max_bb");
        VUIT_Manage("bus_elec_dia_hold_v_text");
        VUIT_Manage("bus_elec_dia_v_hold_label");
        VUIT_Unmanage("bus_elec_dia_b_blank_pix_label");
        VUIT_Unmanage("bus_elec_dia_be_pix_label");
        VUIT_Unmanage("bus_elec_dia_bg_pix_label");
        VUIT_Manage("bus_elec_dia_bq_pix_label");
        VUIT_Unmanage("bus_elec_dia_bs_pix_label");
        VUIT_Unmanage("bus_elec_dia_bx_pix_label");
        VUIT_Unmanage("bus_elec_dia_remote_form");
        VUIT_Unmanage("bus_elec_dia_v_max_label");
        VUIT_Unmanage("bus_elec_dia_v_max_text");
        VUIT_Unmanage("bus_elec_dia_v_min_label");
        VUIT_Unmanage("bus_elec_dia_v_min_text");
        VUIT_Unmanage("bus_elec_dia_phase_label");
        VUIT_Unmanage("bus_elec_dia_phase_text");
    break;

    case 'S':
        if((HashLookup("bus_elec_dia_type_bs_button", &button_id)) == FALSE)
        {
          printf("\nCan't find bus_elec_dia_type_bs_button id\n");
          return;
        }

        XtVaSetValues(option_button_id,
                      XmNlabelString,
                      XmStringCreate("BS", XmSTRING_DEFAULT_CHARSET),
                      NULL);

        VUIT_Manage("bus_info_gen_sched_bb");
        VUIT_Unmanage("bus_info_gen_max_bb");
        VUIT_Manage("bus_elec_dia_hold_v_text");
        VUIT_Manage("bus_elec_dia_v_hold_label");
        VUIT_Unmanage("bus_elec_dia_b_blank_pix_label");
        VUIT_Unmanage("bus_elec_dia_be_pix_label");
        VUIT_Unmanage("bus_elec_dia_bg_pix_label");
        VUIT_Unmanage("bus_elec_dia_bq_pix_label");
        VUIT_Manage("bus_elec_dia_bs_pix_label");
        VUIT_Unmanage("bus_elec_dia_bx_pix_label");
        VUIT_Unmanage("bus_elec_dia_remote_form");
        VUIT_Unmanage("bus_elec_dia_v_max_label");
        VUIT_Unmanage("bus_elec_dia_v_max_text");
        VUIT_Unmanage("bus_elec_dia_v_min_label");
        VUIT_Unmanage("bus_elec_dia_v_min_text");
        VUIT_Manage("bus_elec_dia_phase_label");
        VUIT_Manage("bus_elec_dia_phase_text");
        VUIT_Unmanage("bus_elec_dia_gen_p_label");
        VUIT_Unmanage("bus_elec_dia_gen_p_text");

    break;

    case 'X':
        if((HashLookup("bus_elec_dia_type_bg_button", &button_id)) == FALSE)
        {
          printf("\nCan't find bus_elec_dia_type_bg_button id\n");
          return;
        }

        XtVaSetValues(option_button_id, XmNlabelString,
           XmStringCreate("BG", XmSTRING_DEFAULT_CHARSET), NULL);

        VUIT_Unmanage("bus_info_gen_sched_bb");
        VUIT_Manage("bus_info_gen_max_bb");
        VUIT_Unmanage("bus_elec_dia_hold_v_text");
        VUIT_Unmanage("bus_elec_dia_v_hold_label");
        VUIT_Unmanage("bus_elec_dia_b_blank_pix_label");
        VUIT_Unmanage("bus_elec_dia_be_pix_label");
        VUIT_Unmanage("bus_elec_dia_bg_pix_label");
        VUIT_Unmanage("bus_elec_dia_bq_pix_label");
        VUIT_Unmanage("bus_elec_dia_bs_pix_label");
        VUIT_Manage("bus_elec_dia_bx_pix_label");
        VUIT_Manage("bus_elec_dia_remote_form");
        VUIT_Manage("bus_elec_dia_v_max_label");
        VUIT_Manage("bus_elec_dia_v_max_text");
        VUIT_Manage("bus_elec_dia_v_min_label");
        VUIT_Manage("bus_elec_dia_v_min_text");
        VUIT_Unmanage("bus_elec_dia_phase_label");
        VUIT_Unmanage("bus_elec_dia_phase_text");
    break;


    default:
      printf("Unknown bus type %c\n", bus_type);
      return;
  }

  lookup_and_fill_field( "bus_elec_dia_name_text",    bus_string,  6, 13 );
  lookup_and_fill_field( "bus_elec_dia_kv_text",      bus_string, 14, 17 );
  lookup_and_fill_field( "bus_elec_dia_owner_text",   bus_string,  3,  5 );
  lookup_and_fill_field( "bus_elec_dia_zone_text",    bus_string, 18, 19 );
  lookup_and_fill_field( "bus_elec_dia_load_p_text",  bus_string, 20, 24 );
  lookup_and_fill_field( "bus_elec_dia_load_q_text",  bus_string, 25, 29 );
  lookup_and_fill_field( "bus_elec_dia_shunt_p_text", bus_string, 30, 33 );
  lookup_and_fill_field( "bus_elec_dia_shunt_q_text", bus_string, 34, 37 );


  switch ( bus_type )
  {
    case ' ':
    case 'C':
    case 'V':
      lookup_and_fill_field( "bus_elec_dia_max_p_text",   bus_string, 38, 41 );
      lookup_and_fill_field( "bus_elec_dia_gen_p_text",   bus_string, 42, 46 );
      lookup_and_fill_field( "bus_elec_dia_sched_q_text", bus_string, 47, 51 );
      lookup_and_fill_field( "bus_elec_dia_min_q_text",   bus_string, 52, 56 );
    break;

    case 'E':
    case 'Q':
    case 'G':
    case 'X':
      lookup_and_fill_field( "bus_elec_dia_max_p_text2",   bus_string, 38, 41 );
      lookup_and_fill_field( "bus_elec_dia_gen_p_text2",   bus_string, 42, 46 );
      lookup_and_fill_field( "bus_elec_dia_max_q_text",    bus_string, 47, 51 );
      lookup_and_fill_field( "bus_elec_dia_min_q_text2",   bus_string, 52, 56 );
    break;

    case 'S':
      lookup_and_fill_field( "bus_elec_dia_max_p_text",    bus_string, 38, 41 );
      lookup_and_fill_field( "bus_elec_dia_phase_text",    bus_string, 61, 63 );
      lookup_and_fill_field( "bus_elec_dia_sched_q_text",  bus_string, 47, 51 );
      lookup_and_fill_field( "bus_elec_dia_min_q_text",    bus_string, 52, 56 );
    break;
  }

  switch ( bus_type )
  {
    case 'S':
    case 'E':
    case 'Q':
      lookup_and_fill_field( "bus_elec_dia_hold_v_text",   bus_string, 57, 60 );
    break;

    case 'G':
    case 'X':
      lookup_and_fill_field( "bus_elec_dia_v_max_text",     bus_string, 57, 60);
      lookup_and_fill_field( "bus_elec_dia_v_min_text",     bus_string, 61, 64);
      lookup_and_fill_field( "bus_elec_dia_remote_name_text",bus_string,65, 72);
      lookup_and_fill_field( "bus_elec_dia_remote_kv_text", bus_string, 73, 76);
      lookup_and_fill_field( "bus_elec_dia_remote_pcs_text",bus_string, 77, 79);
    break;
  }
}

/*****************************************************************************\
*	Function: fill_bus_selection_box.c 
*		
*	Author: Dan Clark/Bill Rogers		Modified: Feb 20, 1992
*
*	Called by: "Bus" push button callback (edit_menu_bus_button)
*
\*****************************************************************************/


int fill_bus_selection_box ( widget, tag, callback_data)
Widget		widget;
caddr_t		*tag;
caddr_t		*callback_data;


{

  Widget		id, list_id;
  int i=0;
  XmString bus_record[10000];

  PFBASERECORD keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PFBASERECORD *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PFBASERECORD *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */
  PFBASERECORD *branchvalrec;/* ptr to key for branch search */
  PFBASERECORD *nextbranchrec;/* ptr returned for next branch rec */
  PFBASERECORD *busvalrec;	/* ptr to key for bus search */
  PFBASERECORD *nextbusrec;	/* ptr returned for next bus rec */

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statbus;	/* status returned for bus queries of db */
  DB_STAT statbranch;	/* status returned for branch queries of db */

  if ( bus_box_fill_flag == FALSE )
  {

    if ( ( HashLookup("bus_selection_dialog", &id ) ) == FALSE )
       printf("\nCan't find bus_selection_dialog id\n");

    list_id = (Widget)XmSelectionBoxGetChild( id, XmDIALOG_LIST);

    XtSetSensitive ( (Widget)XmSelectionBoxGetChild( id, XmDIALOG_HELP_BUTTON),
                      False );

    XtSetSensitive ( XmSelectionBoxGetChild( id, XmDIALOG_APPLY_BUTTON),
		     False );
 
    /** retrieve all the records (in key order) and write to output file **/
    stat = db_first(&base_db, &nextrec, BSE_TYPE);
    if (stat != D_FOUND)
    {
      printf("ERROR db_search did not find any records \n");
      return(1);
    }

    busvalrec = &keyvalrecord;
    strncpy(busvalrec->type, "B", 1 );
    strncpy(busvalrec->branch.name1, "            ", 12 );
    stat = db_search( &base_db, &busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

    if ( stat != D_FOUND )
    {
       if(stat == D_EOF) {
          printf("Warning: End of table reached\n");
       } else {
         stat = D_FOUND;
       }
    }
   	
    while(stat == D_FOUND)
    {
      if( nextbusrec->type[0] != 'B' && nextbusrec->type[0] != '+' )
        break;
      /* printf("%s", nextbusrec); */

      bus_record[i] = XmStringCreate( nextbusrec, XmSTRING_DEFAULT_CHARSET);
      i++;

      busvalrec = nextbusrec;     /* set up next index */

      stat=db_next( &base_db, &busvalrec, &nextbusrec, BSE_TYPE_NAME1_BASE1);

    }

    XtVaSetValues( id, XmNlistItems, bus_record, XmNlistItemCount, i, NULL);

    printf("%d items added into bus selection box\n", i );
    bus_box_fill_flag = TRUE;
  }

  return(0);

}

/*****************************************************************************\
*	Function: get_area_selection
*
*		  After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*
*	Author: B.Rogers (from example pp 170, Motif Programming Manual
*		Jan 29, 1991		Modified:
*
\*****************************************************************************/
 
void get_area_selection( w, client_data, cbs )

Widget w;
XtPointer client_data;
XmSelectionBoxCallbackStruct *cbs;

{
  Widget id;
  char msg[256], *prompt, *value, ret_string[80];
  int dialog_type;
    
    switch ( cbs->reason )
    {
	case XmCR_OK:
 	    prompt = "Selection:\n";
	    dialog_type = XmDIALOG_MESSAGE;
	    break;
	case XmCR_NO_MATCH:
	    prompt = "Not a valid selection:\n";
	    dialog_type = XmDIALOG_ERROR;
	    break;
	default:
	    prompt = "Unknown selection:\n";
	    dialog_type = XmDIALOG_ERROR;
    }

    XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &value );
    sprintf(msg, "%s%s", prompt, value );
    printf( "%s%s\n", prompt, value );
    XtFree( value );

/*    if((HashLookup("bus_elec_dia_name_text", &id)) == FALSE)
    {
      printf("\nCan't find bus_elec_dia_name_text id\n");
    }

    stringPart( value, ret_string, 6, 13 );
    XmTextSetString( id, ret_string ); 
*/

 }


/*****************************************************************************\
*	Function: get_branch_selection
*
*		  After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*
*	Author: B.Rogers (from example pp 170, Motif Programming Manual
*		Jan 29, 1991		Modified:
*
\*****************************************************************************/
 
void get_branch_selection( w, client_data, cbs )

Widget w;
XtPointer client_data;
XmSelectionBoxCallbackStruct *cbs;

{
  Widget id;
  char msg[256], *prompt, *value, ret_string[80];
  int dialog_type;
    
    switch ( cbs->reason )
    {
	case XmCR_OK:
 	    prompt = "Selection:\n";
	    dialog_type = XmDIALOG_MESSAGE;
	    break;
	case XmCR_NO_MATCH:
	    prompt = "Not a valid selection:\n";
	    dialog_type = XmDIALOG_ERROR;
	    break;
	default:
	    prompt = "Unknown selection:\n";
	    dialog_type = XmDIALOG_ERROR;
    }

    XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &value );
    sprintf(msg, "%s%s", prompt, value );
    printf( "%s%s\n", prompt, value );
    XtFree( value );

    lookup_and_fill_field( "bal_line_info_name_1_text",     value,  6, 13 );
    lookup_and_fill_field( "bal_line_info_kv_1_text",       value, 14, 18 );
    lookup_and_fill_field( "bal_line_info_name_2_text",     value, 19, 26 );
    lookup_and_fill_field( "bal_line_info_kv_2_text",       value, 27, 30 );
    lookup_and_fill_field( "bal_line_info_owner_text",      value,  3,  5 );
    lookup_and_fill_field( "bal_line_info_id_text",         value, 31, 31 );
    lookup_and_fill_field( "bal_line_info_R_text",          value, 38, 43 );
    lookup_and_fill_field( "bal_line_info_X_text",          value, 44, 49 );
    lookup_and_fill_field( "bal_line_info_G_text",          value, 50, 55 );
    lookup_and_fill_field( "bal_line_info_B_text",          value, 56, 61 );
    lookup_and_fill_field( "bal_line_info_parallels_text",  value, 37, 37 );
    lookup_and_fill_field( "bal_line_info_mileage_text",    value, 62, 65 );
    lookup_and_fill_field( "bal_line_info_nominal_text",    value, 33, 36 );
    lookup_and_fill_field( "bal_line_info_thermal_text",    value, 80, 83 );
    lookup_and_fill_field( "bal_line_info_bottleneck_text", value, 84, 87 );
    lookup_and_fill_field( "bal_line_info_month_in_text",   value, 74, 74 );
    lookup_and_fill_field( "bal_line_info_year_in_text",    value, 75, 76 );
    lookup_and_fill_field( "bal_line_info_month_out_text",  value, 77, 77 );
    lookup_and_fill_field( "bal_line_info_year_out_text",   value, 78, 79 );
    lookup_and_fill_field( "bal_line_info_description_text",value, 66, 73 );
 }


/*****************************************************************************\
*	Function: get_bus_selection
*
*		  After bus_selection popup dialog is shown, this routine
*		  returns the highlight user selection.
*
*	Author: B.Rogers (from example pp 170, Motif Programming Manual
*		Jan 29, 1991		Modified:
*
\*****************************************************************************/
 
void get_bus_selection( list_widget, client_data, cbs )

XtPointer client_data;
XmSelectionBoxCallbackStruct *cbs;

{
  char value_str[133];
  char msg[256], *prompt, *value = value_str;
  int dialog_type;
     
    switch ( cbs->reason )
    {
	case XmCR_OK:
 	    prompt = "Selection:\n";
	    dialog_type = XmDIALOG_MESSAGE;
	    break;
	case XmCR_NO_MATCH:
	    prompt = "Not a valid selection:\n";
	    dialog_type = XmDIALOG_ERROR;
	    break;
	default:
	    prompt = "Unknown selection:\n";
	    dialog_type = XmDIALOG_ERROR;
    }

    XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &value );
    sprintf(msg, "%s%s", prompt, value );
    printf( "%s%s \n", prompt, value );

    fill_bus_dialog( value );

}


/*****************************************************************************\
*	Function: manage_brsb (branch selection box)
*
*
*	Author: B.Rogers
*		Mar 17, 1991		Modified:
*
\*****************************************************************************/

void manage_BrSB ( w, client_data, cbs )
Widget w;
XtPointer client_data;
XmSelectionBoxCallbackStruct *cbs;

{

  char msg[256], *prompt, *value;
  char ret_string[150], branch_type;
  int dialog_type;

  switch ( cbs->reason )
    {
        case XmCR_OK:
            prompt = "Selection:\n";
            dialog_type = XmDIALOG_MESSAGE;
            break;
        case XmCR_NO_MATCH:
            prompt = "Not a valid selection:\n";
            dialog_type = XmDIALOG_ERROR;
            break;
        default:
            prompt = "Unknown selection:\n";
            dialog_type = XmDIALOG_ERROR;
    }

    XmStringGetLtoR( cbs->value, 
		     XmSTRING_DEFAULT_CHARSET, &value );
    sprintf(msg, "%s%s", prompt, value );
    printf( "%s%s\n", prompt, value );
    XtFree( value );


  branch_type = value[0];

  switch(branch_type)
  {
    case 'L':
      VUIT_Manage("ac_L_line_info_form");
      lookup_and_fill_field( "bal_line_info_name_1_text",     value,  6, 13 );
      lookup_and_fill_field( "bal_line_info_kv_1_text",       value, 14, 18 );
      lookup_and_fill_field( "bal_line_info_name_2_text",     value, 19, 26 );
      lookup_and_fill_field( "bal_line_info_kv_2_text",       value, 27, 30 );
      lookup_and_fill_field( "bal_line_info_owner_text",      value,  3,  5 );
      lookup_and_fill_field( "bal_line_info_id_text",         value, 31, 31 );
      lookup_and_fill_field( "bal_line_info_R_text",          value, 38, 43 );
      lookup_and_fill_field( "bal_line_info_X_text",          value, 44, 49 );
      lookup_and_fill_field( "bal_line_info_G_text",          value, 50, 55 );
      lookup_and_fill_field( "bal_line_info_B_text",          value, 56, 61 );
      lookup_and_fill_field( "bal_line_info_parallels_text",  value, 37, 37 );
      lookup_and_fill_field( "bal_line_info_mileage_text",    value, 62, 65 );
      lookup_and_fill_field( "bal_line_info_nominal_text",    value, 33, 36 );
      lookup_and_fill_field( "bal_line_info_thermal_text",    value, 80, 83 );
      lookup_and_fill_field( "bal_line_info_bottleneck_text", value, 84, 87 );
      lookup_and_fill_field( "bal_line_info_month_in_text",   value, 74, 74 );
      lookup_and_fill_field( "bal_line_info_year_in_text",    value, 75, 76 );
      lookup_and_fill_field( "bal_line_info_month_out_text",  value, 77, 77 );
      lookup_and_fill_field( "bal_line_info_year_out_text",   value, 78, 79 );
      lookup_and_fill_field( "bal_line_info_description_text",value, 66, 73 );
      VUIT_Manage("ac_L_line_info_form");

    break;

    case 'T':
      VUIT_Manage("transformer_dialog");
      lookup_and_fill_field( "xfmr_dialog_bus1_name_text",  value, 06, 13 );
      lookup_and_fill_field( "xfmr_dialog_bus1_kv_text",    value, 14, 17 );
      lookup_and_fill_field( "xfmr_dialog_metering_text",   value, 18, 18 );
      lookup_and_fill_field( "xfmr_dialog_owner_text",      value, 03, 05 );
      lookup_and_fill_field( "xfmr_dialog_bus2_name_text",  value, 19, 26 );
      lookup_and_fill_field( "xfmr_dialog_bus2_kv_text",    value, 27, 30 );
      lookup_and_fill_field( "xfmr_dialog_id_text",         value, 31, 31 );
      lookup_and_fill_field( "xfmr_dialog_section_text",    value, 32, 32 );
      lookup_and_fill_field( "xfmr_dialog_MVA_text",        value, 33, 36 );
      lookup_and_fill_field( "xfmr_dialog_R_text",          value, 38, 43 );
      lookup_and_fill_field( "xfmr_dialog_X_text",          value, 44, 49 );
      lookup_and_fill_field( "xfmr_dialog_G_text",          value, 50, 55 );
      lookup_and_fill_field( "xfmr_dialog_B_text",          value, 56, 61 );
      lookup_and_fill_field( "xfmr_dialog_parallels_text",  value, 37, 37 );

      lookup_and_fill_field( "xfmr_dialog_thermal_text",    value, 80, 83 );
      lookup_and_fill_field( "xfmr_dialog_emergency_text",  value, 84, 87 );
      lookup_and_fill_field( "xfmr_dialog_bottleneck_text", value, 88, 91 );
      lookup_and_fill_field( "xfmr_dialog_month_in_text",   value, 74, 74 );
      lookup_and_fill_field( "xfmr_dialog_year_in_text",    value, 75, 76 );
      lookup_and_fill_field( "xfmr_dialog_month_out_text",  value, 77, 77 );
      lookup_and_fill_field( "xfmr_dialog_year_out_text",   value, 78, 79 );

      switch( (char)value[1] )
      {
        case ' ':
          VUIT_Unmanage("xfmr_dialog_phase_text");
          VUIT_Unmanage("xfmr_dialog_phase_label");
          VUIT_Manage("xfmr_dialog_tap1_kv_text");
          VUIT_Manage("xfmr_dialog_tap2_kv_text");
          VUIT_Manage("xfmr_dialog_tap1_label");
          VUIT_Manage("xfmr_dialog_tap2_label");
          lookup_and_fill_field( "xfmr_dialog_tap1_kv_text", value, 62, 66 );
          lookup_and_fill_field( "xfmr_dialog_tap2_kv_text", value, 67, 71 );
        break;

        case 'P':
          VUIT_Unmanage("xfmr_dialog_tap1_kv_text");
          VUIT_Unmanage("xfmr_dialog_tap2_kv_text");
          VUIT_Unmanage("xfmr_dialog_tap1_label");
          VUIT_Unmanage("xfmr_dialog_tap2_label");
          VUIT_Manage("xfmr_dialog_phase_text");
          VUIT_Manage("xfmr_dialog_phase_label");
          lookup_and_fill_field( "xfmr_dialog_phase_text",   value, 62, 66 );
        break;

      }
    break;

 }

}

/*****************************************************************************\
*       Function: lookup_and_get_field.c
*
*       Author: B Rogers      Modified: Mar 18, 1992
*
*       Called by:
*
\*****************************************************************************/

void lookup_and_get_field( char *fld_name, char *ret_string )
{
  Widget id;
  int j=0, lbk=0;

  if((HashLookup( fld_name, &id)) == FALSE)
  {
    printf("\nCan't find %s id\n", fld_name );
  }

  strcpy ( ret_string, XmTextGetString(id) );
/*  printf(" data from %s is %s\n", fld_name, ret_string );*/


}

/*****************************************************************************\
*       Function: change_field.c
*	 	  examine a field and if change, replace entire field
*		  with new data.  If no change, then blank out the field.
*
*       Author: B Rogers      Modified: Mar 25, 1992
*
*       Called by: build_bus_record.c
*
\*****************************************************************************/

void change_field( char *old_fld, char *chg_field, char *fld_name,
					   int col_1, int end_col, 
					   int dec_pt )
{
   char old_value[20], new_value[20], c20[5];
   char vfrmt[7];
   int i, j, len;
   
   lookup_and_get_field(  fld_name, new_value );

   stringPart ( old_fld, old_value, col_1, end_col );

   len = end_col-col_1+1;
   if( len <= 0 || len>=20 )
     printf(" ERROR * attempt to build field (%s) with bad column numbers %d,%d \n", fld_name, col_1, end_col );

   if ( len<=9 )
      sprintf( vfrmt, "%%%1d.%1df", len, dec_pt );
   else
      sprintf( vfrmt, "%%%2d.%1df", len, dec_pt );

   if ( atof( old_value ) != atof( new_value ) )
   {
      sprintf( c20, vfrmt, atof(new_value) );
      strncat( chg_field, c20, len );
   } 
   else
   {
      strcat( chg_field, "                    " );
   }

   chg_field[end_col+1] = 0;
}
