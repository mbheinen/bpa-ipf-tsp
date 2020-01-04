static char sccsid[]="@(#)pscordat.c	20.69 5/11/98";
/*****************************************************************************\
* file: pscordat.c
* author: Dan Clark
* date: Dec 29, 1992
* purpose: functions to handle metric cor file data
*
* Referenced by: base_data.c
*		  coord_data.c
*		  edge.c
*		  graphbase.c
*		  graphcor.c
*		  graphdata.c
*		  graphpscor.c
*		  guilib.m
*		  libgui.m
*		  printopts.c
*		  pscordat.c
*		  toolbox.c
*
\*****************************************************************************/
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/SelectioB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include "dmgr.h"
#include "coord_data.h"
#include "graph_data.h"
#include "base_out.h"
#include "EdgeG.h"
#include "ipfdebug.h"
#include "em.h"
#include "ipf_ipc.h"    /* added 2/96 to get the ENDOFMSG key */

em_init			/* "special initialization for error box" */

#define min(A,B) ((A) < (B) ? (A):(B)) /* if A<B return A, otherwise return B */
#define max(A,B) ((A) > (B) ? (A):(B))

#define BUS_COORD_DIALOG  0
#define GENERAL_DIALOG    1
#define COMMENT_DIALOG    2
#define MAXBENDS	 30
#define DegtoRad        .0174532

DBID pscor_db;		/* see coord_data.h for structure */
static FILE *coordfptr;	/* ptr to file returned by psguiOpenCoordFile */
char v_flag;		/* relative position of name to bus symbol */
static int card_index;	/* number of coord file card read */
static int which_display;  /* tells which dialog is active (bus or general)*/
static int pixel_flag;	/* flag whether to write float or integer number */
static Boolean coord_display_update= FALSE;
GraphElement *org_cmmt_ge;
GraphElement *cmmt_ge;
static GraphElement *edt_edge;
static GraphElement *cmplx_edge;
static Boolean set_flow_enabled;

static char     outbuf_array[BUFSIZE];
static char     inbuf_array [BUFSIZE];

extern icon_bus_width;
extern icon_bus_height;

int  pscreatecoordtbl		();
Boolean psopenCoordFile		( char *filename );
int  psreadCoordData		();
void convertCoordRec		( PSCoordFileRecord *prec);
int  psbendCount		( PSBranchRecord *rec );
void reverseSegment		( PSBranchRecord *prec, int bendcnt );
void psreverseCard		( PSBranchRecord *prev, PSBranchRecord *porg );
int  pswriteCoordFile		( char *coord_output_file_name );
int  psfindfirstcoordbusrec	( PSBusRecord **nextbusrec );
void display_bus_coord_data	( char *busname );
PSCoordFileRecord *coord_db_bus_search( char *busname );
PSCoordFileRecord *coord_db_branch_search( char *busname1, char *busname2 );
void write_coord_plot_dlg_to_db ( Widget, XtPointer, XmAnyCallbackStruct * );
int psprintCoordData		( Widget, XtPointer, XmAnyCallbackStruct * );
int psprintcoord_db		( char type );
Boolean bad_coord_sb		(Widget *id, Widget *list_id, Widget *text_id );
void cor_selection_edit		( Widget widget, char *tag,
					XmSelectionBoxCallbackStruct *cb);
void fill_cor_edit_selection ( Widget widget, char *tag, char *callback_data);
void modifyBusLocation	( Widget w, XtPointer tag,
                                   XmAnyCallbackStruct *cbs );
void display_graph_coord_data	( GraphElement *pvertex );
void display_coord_data		( GraphElement *vertex );
void put_coord_value_in_text_box( char *field_name, double value );
void get_gg_xy_values		( GraphElement *ge, int *x, int *y );
char *get_gg_string		( GraphElement *pvertex );
int gdgt_ge_x_offset		( GraphElement *ge );
int gdgt_ge_y_offset		( GraphElement *ge );
void activate_coord_update	( Widget w, XtPointer tag,
				  XmAnyCallbackStruct *cbs );
void deactivate_coord_update	( Widget w, XtPointer tag,
				  XmAnyCallbackStruct *cbs );
GraphElement *get_comment_link_by_class( GraphElement *pg, GraphClass class );
/*void list_comment_strs	();	   */
/*void list_nth_comment		( int n ); */
void fill_coord_form		( char *, char );
int ipc_synch_rw		( char *rtntext, char *text );
void process_update_data	( char *pfdataret );
int update_bus			( char *, char * );
/* void  write_out_draw_cards	( FILE *fptr );
* int walk_the_line		( GraphElement *bends[], int endtyp ); */
void get_file_save_option	(int *save_opt);
void set_volts_relative_of_name (Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs );
void set_expose_occured         ();
void put_vtx_xy_values_in_text_box( GraphElement *ge, 
				    char *x_fld, char *y_fld );
void adjust_bus_angle	  ( GraphElement *, char *,  char *, GraphClass, int );
void turnOffGenReac		( GraphElement *pgraphbus, int class );



extern double zoom_factor;
extern void  set_units_to_cm	();
extern void set_origin_to_bottom();
extern int HashLookup(char *name, Widget *id);
extern char *cstring_to_string	( XmString motif_string );
extern int getCurBus          ( char **namebase );
extern Boolean get_widget_text( char *fld_name, char *ret_string, int size );
extern Widget widget_id		();
extern int reformat		(char *, char *, char *);
extern void lookup_and_get_field( char *, char *, int );
extern int  option_setting	( char *wid_name );
extern void set_button_state	( char *widget_name, Boolean v1 );
extern void lookup_and_get_field( char *, char *, int );
extern void lookup_and_fill_field( char *, char *, int, int );
extern void add_blanks_if_short	( char *, int, char * );
extern int  psgetHtWdScale	();
extern int  atoi_cnt_zero	( char *str, int size );
extern int atoi_cnt		( char *str, int size );
extern void cascade_pd_label_set( char *, char * );
extern void write_out_option_cards( FILE  *fptr );
extern int write_out_define_cards( FILE  *fptr );
extern double MOTIF_to_ps_x	( int x, char *, char *, char * );
extern double MOTIF_to_ps_y	( int y, char *, char *, char * );
extern DB_STAT findBusGraphVertexRec( char *namebase, GraphElement **pe );
extern void strcat_field	( char *, char *, int, char *fmt );
extern Vuit_MU			( char *, char * );
extern void disable_pushbutton  ( char * );
extern void enable_pushbutton   ( char * );
extern void refresh_comments	();
extern void send_comments_and_defines_to_pf();
extern void clip_trailing_blanks( char * );
extern void get_box_size_of_string ( char *str, int *wd, int *ht);
extern void lookup_and_fill_field_w_dec( char *, char *, int, int, int );
extern void lookup_and_fill_field      ( char *, char *, int, int );
extern Widget lookup_kv_wid	(double pct);
extern void refreshBusImage	(pfOutRecord *);
extern void get_bus_name_solution_opt (int *state);
extern int  forEachipfLine ( char *, int (*do_fnct)( char *, char *), char *);
extern int  isbus		( pfOutRecord *);
extern double atof_cnt_zero	( char *, int );
extern double radiansBetweenPts	( int x, int y, int x2, int y2 );


long pscmp_key_name_base(void *a, void *b, long len)
/*****************************************************************************\
*	KEY-NAME-BASE sort key for coord_db
*	determines which record is 1st alphabetically
\*****************************************************************************/
{
  PSCoordFileRecord *keyrec;
  PSCoordFileRecord *checkrec;
  int rtncmp1;
  int rtncmp2;

  keyrec = (PSCoordFileRecord *)a;
  checkrec = (PSCoordFileRecord *)b;
  
  if(ipfdebug & DB_GraphPSCorMask) {
    fprintf(ipfdbio, "pscmp_key_name_base: keyrec %s\n", keyrec);
    fprintf(ipfdbio, "pscmp_key_name_base: checkrec %s\n", checkrec);
  }

  rtncmp1 = 
     strncmp(keyrec->bus.key, checkrec->bus.key, sizeof(keyrec->bus.key));
  rtncmp2 = strncmp(keyrec->bus.name, checkrec->bus.name, 
     sizeof(keyrec->bus.name) + sizeof(keyrec->bus.base) );

  if(rtncmp1 > 0)
     return(1);
  else
     if(rtncmp1 < 0)
        return(-1);
  else
     if(rtncmp2 > 0)
        return(1);
  else
     if(rtncmp2 < 0)
        return(-1);
  else
     return(0);
}


long pscmp_key_idx(void *a, void *b, long len)
/*****************************************************************************\
*	Sort on field key and on integer field idx for coord_db
\*****************************************************************************/
{
  PSCoordFileRecord *keyrec;
  PSCoordFileRecord *checkrec;
  int rtncmp1;
  int rtncmp2;

  keyrec = (PSCoordFileRecord *)a;
  checkrec = (PSCoordFileRecord *)b;
  if(ipfdebug & DB_GraphPSCorMask) {
    fprintf(ipfdbio, "pscmp_key_idx: keyrec %s\n", keyrec);
    fprintf(ipfdbio, "pscmp_key_idx: checkrec %s\n", checkrec);
  }

  rtncmp1 = strncmp(keyrec->bus.key, checkrec->bus.key, 
     sizeof(keyrec->bus.key));
  rtncmp2 = (int)(keyrec->bus.idx - checkrec->bus.idx);

  if(rtncmp1 > 0)
     return(1);
  else
     if(rtncmp1 < 0)
        return(-1);
  else
     if(rtncmp2 > 0)
        return(1);
  else
     if(rtncmp2 < 0)
        return(-1);
  else
     return(0);
}


PSBranchRecord ppsbranch;
/* first character in any record */
/* d - dup keys, c - char, 1 length, 0 start pos */
static SCHEMA coord_sch[] =
{
  { 'n', 'c', sizeof(PSCoordFileRecord), 0 },     /* size of record stored */
  { 'd', 'c', sizeof(ppsbranch.key),
	      offsetof(PSBranchRecord, key )},	  /* COORD_KEY */
  { 'd', 'c', sizeof(ppsbranch.name1) + sizeof(ppsbranch.base1),
	      offsetof(PSBranchRecord, name1)},	  /* COORD_NAME1_BASE1 */
  { 'd', 'c', sizeof(ppsbranch.name2) + sizeof(ppsbranch.base2),
	      offsetof(PSBranchRecord, name2 )},	 /* COORD_NAME2_BASE2 */
  { 'd', 'u', sizeof(PSCoordFileRecord), 0, 0, pscmp_key_name_base }, 
						  /* COORD_KEY_NAME1_BASE 1 */
  { 'u', 'i', sizeof(ppsbranch.idx),
             offsetof(PSBranchRecord, idx )},	  /* COORD_IDX */
  { 'u', 'u', (long)sizeof(PSCoordFileRecord), 0, 0, pscmp_key_idx}, 
						   /* COORD_KEY_IDX */
  { 'd', 'c', sizeof(ppsbranch.name1) + sizeof(ppsbranch.base1) 
              + sizeof(ppsbranch.name2) + sizeof(ppsbranch.base2),
              offsetof(PSBranchRecord, name1) } 
              /* COORD_NAME1_BASE1_NAME2_BASE2 */
};

#define COORD_KEY_CNT (int)(sizeof(coord_sch)/sizeof(coord_sch[0]))
#define COORD_PARTITIONS 0


int pscreatecoordtbl()
/*********************************************************************\
*	Purpose: create a new db table for coordinate data
*	Author: Dan Clark	Dec, 1991	Modified:
*	Called by: apply_files (filedlgrtn.c) 
\*********************************************************************/
{
  DB_STAT stat;			/* status returned from dmgr calls */

  if(db_valid_db(&pscor_db) == D_OKAY)  {
    stat = db_destroy(&pscor_db);
    assert(stat == D_OKAY);
  }

  stat = db_create( (SCHEMA *)coord_sch,	/* db name	*/
		    COORD_KEY_CNT,		/* 8 fields	*/
		    COORD_PARTITIONS,		/* 0 partitions */
		    &pscor_db); 		/* schema name	*/
  assert(stat == D_OKAY);
  card_index = 0;

  return((int)stat);



}
Boolean psopenCoordFile( char *filename )
/*********************************************************************\
*	Purpose: Open coordinate file
*	Author: Dan Clark	Dec, 1991	Modified:
*	Called by: apply_files (filedlgrtn.c) 
\*********************************************************************/
{
  if((coordfptr = fopen(filename, "r")) == 0) 
  {
    sprintf(errmsg, "=** UNABLE TO OPEN COORDINATE FILE %s for READ **=\n",
         filename);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "openCoordFile";
    err.type = WARNING;
    err.ident = sccsid;
    em_show( &err );
    return(False);
  }
  return(True);
}



Boolean pscloseCoordFile()
/*************************************************************************\
* Purpose: close the coordinate file ***
*
\*************************************************************************/
{
  fclose (coordfptr);
  coordfptr = NULL;

  return(0);
}


int psreadCoordData()
/*********************************************************************\
* Purpose: read coordinate file data into a dbmgr table
*	see INTERACTIVE POWERFLOW - DETAILED DESIGN DOCUMENT
*		"read coord file" for better explaination.
*		
*	Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*
*	Called by: apply_files (filedlgrtn.c)
\*********************************************************************/
{
  PSCoordFileRecord coord_rec;	/* allocate space for coord data */
  PSCoordFileRecord *rec;	/* allocate 1 space for ptr  */
  PSCoordFileRecord *junk;	/* ptr to new coord rec in db (not needed) */

  PSCoordFileRecord keyrec;	/* allocate space for data */
  PSCoordFileRecord *key;	/* allocate space for ptr  */

  char card_data[256];		/* card data directly from coordinate file */

  int len;
  DB_STAT stat;			/* status returned from dmgr calls */

  rec = &coord_rec;

  if(coordfptr == NULL) {
     printf("ERROR no coordinate file NAME available.\n");
     return(-1);
  }

  /*** copy ALL data from coordfile to pscor_db dmgr ***/
  while ((fgets( card_data, 132, coordfptr )) != NULL)
  {
				/** zero entire receiving area **/
    memset(&rec->cor.idx, '\0', sizeof(PSCoordFileRecord));

    len = min(strlen(card_data), 
            sizeof(PSCoordFileRecord) - sizeof(rec->cor.idx) - 1 );

    rec->cor.idx    = (long)card_index++;	/* Create numerial index    */
    memcpy(&rec->cor.key[0], card_data, len);	/* copy coord rec to struct */
    rec->cor.eol[0] = '\0';			/* Add EOL		    */

    convertCoordRec(rec);

    /*************************** check for dup ***************************/
    stat = db_search(&pscor_db, &rec, &junk, COORD_KEY_NAME1_BASE1 );
    if( D_FOUND == stat && rec->cor.key[0]=='B' )
    {
      printf("** INFO ** duplicate coordinate bus record ignored\n%s\n",
		 &rec->cor.key[0] );
      sprintf(errmsg, "=** DUPLICATE BUS RECORD IN COORDINATE FILE **=\n%s\n",
         &rec->cor.key[0]);
      err.line = EM_LINE;
      err.msg  = errmsg;
      err.link = "psreadCoordData";
      err.type = INFO;
      err.ident = sccsid;
      em_show( &err );
    }
    else
    {
      stat = db_insert(&pscor_db, &rec, &junk);
      if(stat != D_OKAY) {
        printf("ERROR coordinate record insert pscor_db table stat %d\n",stat);
        printf("  record failed is %ld %s\n", rec->cor.idx, &rec->cor.key[0]);
        assert(stat == D_OKAY);
      }
    }
  }

/******************* CHECK FOR CORRECT COORD FORMAT **************************/
/* ( 1st coord record should begin with "[ID COORD" otherwise its old format */

  /*** retrieve first "I" ([ID COORD) type record ***/
  key = &keyrec;
  strncpy(key->cor.key, "[", 1);

  stat = db_search( &pscor_db, &key, &junk, COORD_KEY );

  if (stat != D_FOUND)
  {
            /** if no I record is found assume it is in old style format **/
    sprintf(errmsg, "%s\n%s\n%s\n",
      "Coordinate file does not have [ID COORD  record",
      " Probably a file in the old format",
      " Please convert and re-read file");
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "psReadCoordData";
    err.type = FATAL;
    err.ident = sccsid;
    em_show( &err );
    stat = db_destroy(&pscor_db);
  }

  if(ipfdebug & DB_GraphPSCorMask) {
    printf("%d coord file cards read.\n", card_index);
    psprintCoordData( 0, 0, 0 ); 
  }

  return(0);
}

void convertCoordRec( PSCoordFileRecord *prec)
/*******************************************************************
* look at the input record and:
*	1) convert alpha basekv to floating pt
*	2) reverse line names if backwards
*	3) change P (Font) cards to CP cards (Comment with Font subkey)
*
*	Called by: psreadCoordData (above)
********************************************************************/
{
  int keysize;
  char base4[5];

  keysize = sizeof(prec->branch.name1) + sizeof(prec->branch.base1);

   switch(prec->bus.key[0])
   {
     case 'B':
        reformat(prec->bus.base, base4, "%f4.0"); /* Warning - don't copy */
	strncpy( prec->bus.base, base4, 4 );	  /* direct to prec->bus.base */
     break;

     case 'L':
     case 'T':
        reformat( prec->branch.base1, base4, "%f4.0" );
	strncpy( prec->branch.base1, base4, 4 );
        reformat( prec->branch.base2, base4, "%f4.0" );
	strncpy( prec->branch.base2, base4, 4 );
        if( strncmp(prec->branch.name1, prec->branch.name2, keysize) > 0)
        {
          psreverseCard(&prec->branch, &prec->branch);
        }
     break;

     case 'P':	/* Font cards must be kept in same order as Comment cards */
	prec->comment.key[0]    = 'C';
	prec->comment.subkey[0] = 'P';
     break;
  }
}


int psbendCount (PSBranchRecord *rec )
/****************************************************************************\
*  Purpose: counts number of bend x-coords on an 'L' or 'T' record.
*
*  Called by: psreverseCard
*	      psreverseCoordCard	(graphdata.c)
*	      pscreateGraphCorEdges	(graphdata.c)
*	      psfindGraphMax 		(graphdata.c)
\****************************************************************************/
{
#define SPACE "            "

  int bendcnt = 0;
  int i;

  if( strlen( &rec->key[0] ) < 89 )
  {
 printf("**WARNING** from psbendCount (pscordat.c)\n" );
 printf("  Length of LINE or TRANSF record should be 90 characters long:\n%s\n",
	rec->key );
  }

  if( rec->key[0] == 'L' || rec->key[0] == 'T' )
  {
    for(i = 0; i < PSMAXBENDS; ++i)
    { 
      if( 0 != strncmp(rec->bend[i].x, SPACE, sizeof(rec->bend[i].x)) &&
          0 != strncmp(rec->bend[i].y, SPACE, sizeof(rec->bend[i].y)) )
      {
        bendcnt += 1;	 /* count bends */ 
      }
    }
  }
  else
  {
    printf("*** PROGRAMMER ERROR *** in psbendCount ( pscordat.c )\n");
    printf("          pointer does not point to a valid line record.\n");
    printf("          %60.60s\n", rec->key );
  }

  return ( bendcnt );

}

void reverseSegment(PSBranchRecord *prec, int bendcnt)
/***************************************************************************\
*
*
\***************************************************************************/
{
  int curseg;
  char newsegtxt[12];

  curseg = atoi_cnt_zero(prec->segment, 2);
  if(ipfdebug & DB_GraphPSCorMask) {
    printf("reverseSegment: bendcnt %d prev %2.2s %d ", 
          bendcnt, prec->segment, curseg );
  }
  curseg = max(1 + bendcnt - curseg, 0) + 1;
  sprintf(newsegtxt, "%.2d", curseg);
  strncpy(prec->segment, newsegtxt, (int)sizeof(prec->segment) );
  if(ipfdebug & DB_GraphPSCorMask) {
    printf("  new %2.2s %d\n", prec->segment, curseg );
  }
}


void psreverseCard(PSBranchRecord *prev, PSBranchRecord *porg)
/****************************************************************************\
* Purpose: to put the coordinate cards in standard order 
* 
* NOTE: this routine works only because the 
* line card and the transformer card happen to have the
* same location for the bend points!
* a better routine would take into account the possible
* variation in the format of the cards.
*
* Author: Dan Clark
\****************************************************************************/
{
  PSBranchRecord tmp;
  PSBranchRecord *ptmp = &tmp;

  int bendcnt;
  int i;
  int keysize;
  int possize;

  keysize = sizeof(porg->name1) + sizeof(porg->base1);
  possize = sizeof(porg->bend[0].x);

  memcpy(ptmp, porg, sizeof(PSBranchRecord));
  memcpy(ptmp->name1,porg->name2, keysize);
  memcpy(ptmp->name2,porg->name1, keysize);

  bendcnt = psbendCount( porg );

  for(i = 0; i < bendcnt; i++) {
    assert((bendcnt-i-1) >= 0);
    memcpy(ptmp->bend[i].x, porg->bend[bendcnt-i-1].x, sizeof(ptmp->bend[i].x));
    memcpy(ptmp->bend[i].y, porg->bend[bendcnt-i-1].y, sizeof(ptmp->bend[i].y));
  }

  reverseSegment(ptmp, bendcnt);

  memcpy(prev, ptmp, sizeof(PSBranchRecord));

}

#define SAVE_ALL 1
#define SAVE_NO 0


void get_file_save_option(int *save_opt)
/*************************************************************************\
\*************************************************************************/
{
  Widget wid;

  if((HashLookup("save_coord_delete_extra_data", &wid)) != False)
  {
    if(XmToggleButtonGadgetGetState(wid))
    {
      *save_opt = SAVE_NO;
    }
    else
    {
      *save_opt = SAVE_ALL;
    }
  }
  else
  {
    *save_opt = SAVE_ALL;	/* widget not found - default to SAVE_ALL */
  }
}


int pswriteCoordFile (char *coord_output_file_name)
/*****************************************************************************\
*	Function: WRITECOORDFILE
*  		open and write the coordinate file
*		
*	Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
*       Modified: John Rutis	June, 1992
*
*	Called by: 
*
\*****************************************************************************/
{
  FILE *fptr;		/* ptr to file returned by open */

  PSCoordFileRecord keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PSCoordFileRecord *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PSCoordFileRecord *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */
  PSCoordFileRecord *branchvalrec;/* ptr to key for branch search */
  PSCoordFileRecord *nextbranchrec;/* ptr returned for next branch rec */
  PSCoordFileRecord *busvalrec;	/* ptr to key for bus search */
  PSCoordFileRecord *nextbusrec;	/* ptr returned for next bus rec */

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statbus;	/* status returned for bus queries of db */
  DB_STAT statbranch;	/* status returned for branch queries of db */
  int numCards;			/* the number of coordinate file cards read */
  int numBus, numBr, numCom;
  int numUnkn;
  char *pcmt_str;

  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  GraphElement *pvertex;

  GraphElement *linkgraphrec;
  GraphLink *llretrec;

  DB_STAT stat_link;            /* status returned for ll dmgr access */
  int save_opt;
  int lines_on;
  static int bus_coord_fetched = 0;

  /* open coordinate file */

  if ( ( fptr = fopen(coord_output_file_name, "w" ) ) == NULL ) 
  {
              extern XtAppContext app_context;
          static int answer;

          Vuit_MU( "M", "write_protected_file_msg"  );
                /* hold up - until user pushes a pushbutton */
          while (answer == 0) XtAppProcessEvent(app_context, XtIMAll);
 
    sprintf(errmsg, "=** UNABLE TO OPEN COORDINATE FILE %s FOR WRITING **=\n",
       coord_output_file_name);
    err.line = EM_LINE;
    err.msg  = errmsg;
    err.link = "writeCoordFile";
    err.type = WARNING;
    err.ident = sccsid;
    em_show( &err );
    return(-1);
  }

  /* ensure those debug radio buttons aren't in the wrong position! */
  /* Presently we can only save SCALED, METRIC, with origin at BOTTOM */
/* note: these are not read by save routines 
*   if(bus_coord_fetched == 0) {
*    fetch_widget("modify_bus_coord_dia");
*    bus_coord_fetched = 1;
*  }
*  set_button_state( "graph_unit_cm_rb",       True );
*  set_button_state( "graph_origin_bottom_rb", True );
*  set_button_state( "graph_unit_scaled_rb",   True );
*/
  set_units_to_cm();            /* hardwire to save as cm data */
  set_origin_to_bottom();

  psgetHtWdScale();

  /** first write the key record which the plotter expects ***/

  fprintf(fptr, "%s\n", PSIDRECORD);

  numBr    = 0;
  numBus   = 0;
  numCom   = 0;
  numUnkn  = 0;
  numCards = 1;

  if(db_valid_db(&pscor_db) != D_OKAY)
  {
   /* printf("Warning: no pscor_db originally opened\n"); CREATE FROM SCRATCH */
  }
  else
  {
    /** for unhandled record types retrieve records and write to output file **/
    keyvalrec = &keyvalrecord;
    keyvalrec->cor.idx = (long)1;
    stat = db_first(&pscor_db, &nextrec, COORD_IDX);
    if (stat != D_FOUND) 
    {
      printf("WARNING db_search no idx %ld records \n", keyvalrec->cor.idx); 
    } 
    while(stat == D_FOUND)
    {
      if(0 == memcmp(&nextrec->cor.key[0], PSKEY_BUS,
				 sizeof(nextrec->cor.key)) ||
         0 == memcmp(&nextrec->cor.key[0], PSIDRECORD,
				 sizeof(nextrec->cor.key) ) || 
         0 == memcmp(&nextrec->cor.key[0], PSKEY_TRANSFORMER,
				 sizeof(nextrec->cor.key) ) ||
         0 == memcmp(&nextrec->cor.key[0], PSENDRECORD,
				 sizeof(nextrec->cor.key) ) ||
         0 == memcmp(&nextrec->cor.key[0], PSKEY_OPTION,
				 sizeof(nextrec->cor.key) ) ||
         0 == memcmp(&nextrec->cor.key[0], PSKEY_COMMENTS,
				 sizeof(nextrec->cor.key) ) ||
         0 == memcmp(&nextrec->cor.key[0], PSKEY_DEFINES,
				 sizeof(nextrec->cor.key) ) ||
         0 == memcmp(&nextrec->cor.key[0], "  ",
				 sizeof(nextrec->cor.key) ) ||
         0 == memcmp(&nextrec->cor.key[0], PSKEY_LINE,
				 sizeof(nextrec->cor.key)))
      {
        /* skip these records */
      }
      else
      {
        int recsize;
        recsize = strlen(&nextrec->cor.key[0]) - 1;     
        fprintf(fptr, "%*.*s\n", recsize, recsize, &nextrec->cor.key[0]);
        numCards++;
	numUnkn++;
      }
      keyvalrec = nextrec;
      stat = db_next(&pscor_db, &keyvalrec, &nextrec, COORD_IDX);
    }
  }

  write_out_option_cards( fptr );

  /* write_out_draw_cards  ( fptr ); */

  write_out_define_cards( fptr );

/***************************************************************************\
***************  PART II - COMMENT strings FROM graph_db ********************
\***************************************************************************/

  stat = db_valid_db(&graph_db); /*** check for a valid db **/
  if(stat != D_INVALID_DB) {	 
    /************ Get first COMMENT record ********************************/
    pgraphnode = &graphnode;
    pgraphnode->type = GraphTypeVertex;
    pgraphnode->class = GraphClassVertexComment;
    pgraphnode->idx = (long)0;
    stat = db_search( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
  }

  stat = D_FOUND;

  /***** loop through all COMMENT records **********************************/
  while(stat == D_FOUND)
  {
    if( GraphClassVertexComment != pgraphnodenxt->class  )
    {
      break;
    }
      if(pgraphnodenxt->wid == NULL)
      {
        fprintf(stderr, "Error - no comment wid for C %6.2lf%6.2lf/n", 
	MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ));
      }
      else
      {
        /* first check for FONT card that must precede this */
        pvertex = get_comment_link_by_class( pgraphnodenxt,
			 GraphClassVertexFont );
        if( pvertex != NULL )
        {
          XmString xstr;
          numCards++;
          XtVaGetValues(pvertex->wid, XmNlabelString, &xstr, NULL);
          pcmt_str = cstring_to_string(xstr);
	  if( pcmt_str != NULL )
	  {
	    clip_trailing_blanks( pcmt_str );
		/*temp fix - ensure SOMETHING appears on screen */
	    if( pcmt_str[0] == '\0' ) strcpy( pcmt_str, "[]" );
	    fprintf(fptr, "P %s\n", &pcmt_str[2] ); /*Change 'CP' to 'C '*/
	  }
	  else
	  {
	    printf("PROGRAM ERROR - from pswriteCoordFile (pscordat.c)\n" );
	    printf("  NULL pointer to comment widget labelString\n" );
	    fprintf(fptr, "C %6.2lf%6.2lf%s\n",
	    MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	    MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ),
	    "(Null)" );
	  }
        }

        pvertex = get_comment_link_by_class( pgraphnodenxt,
					 GraphClassVertexOrgComment );

	if(pvertex == NULL)
	{
	  fprintf(stderr, "Error - no original wid for C %6.2lf%6.2lf\n", 
	  MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	  MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ));
	}
	else
	{
	  XmString xstr;
          numCards++;
          numCom++;
	  XtVaGetValues(pvertex->wid, XmNlabelString, &xstr, NULL);
          pcmt_str = cstring_to_string(xstr);
	  if( pcmt_str != NULL )
	  {
	    clip_trailing_blanks( pcmt_str );
		/*temp fix - ensure SOMETHING appears on screen */
	    if( pcmt_str[0] == '\0' ) strcpy( pcmt_str, "[]" );
	    fprintf(fptr, "C %6.2lf%6.2lf%s\n",
	    MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	    MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ),
	    pcmt_str );
	  }
	  else
	  {
	    printf("PROGRAM ERROR - from pswriteCoordFile (pscordat.c)\n" );
	    printf("  NULL pointer to comment widget labelString\n" );
	    fprintf(fptr, "C %6.2lf%6.2lf%s\n",
	    MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	    MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ),
	    "(Null)" );
	  }
		/* Don't use XmStringFree!  It will destroy the data! */
        }
      }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
  }


/***************************************************************************\
***************  PART III - BUS AND BRANCH DATA FROM graph_db ***************
\***************************************************************************/

  /** now print graph records **/
  /* printGraphData(0,fptr,0); */

  get_file_save_option(&save_opt);

  /************ Get first BUS record ********************/
  pgraphnode = &graphnode;
  stat = db_first ( &graph_db, &pgraphnodenxt, GRAPH_TYPE_VERTEX_ID);

  /***** loop through all BUS records ***************************************/
  while(stat == D_FOUND)
  {
    if( GraphTypeVertex == pgraphnodenxt->type 
        && GraphClassVertexBus == pgraphnodenxt->class ) 
    {
     if(SAVE_ALL == save_opt || GraphDisplayOn == pgraphnodenxt->display) {
       numCards++;
       numBus++;
       printPSBusRecord(fptr, pgraphnodenxt);
     }
    }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt,
             GRAPH_TYPE_VERTEX_ID);
  }

  /*************** Get first BRANCH record ********************************/
  if(stat != D_INVALID_DB) {	 
    pgraphnode = &graphnode;
    stat = db_first ( &graph_db, &pgraphnodenxt,
                    GRAPH_TYPE_VERTEX_FARVERTEX);
  }
  /********** loop through BRANCH records *********************************/
  while(stat == D_FOUND)
  {
    if( GraphTypeEdge == pgraphnodenxt->type &&
	 GraphClassEdgeComplexSection == pgraphnodenxt->class &&
	 strncmp( pgraphnodenxt->farvertex_id, "ZZ", 2 ))   /* don't save */
    {
      lines_on = checkLineOn(pgraphnodenxt);
      if(SAVE_ALL == save_opt || lines_on > 0) {
        printPSBranchRecord(fptr, pgraphnodenxt);
      }
    }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt,
             GRAPH_TYPE_VERTEX_FARVERTEX);
  }

  fprintf(fptr, "%s\n", PSENDRECORD);

  fclose (fptr);
  fptr = NULL;

  sprintf(errmsg, " %d Coordinate cards written to %s\n", 
		    numCards, coord_output_file_name);

  err.line = EM_LINE;
  err.msg  = errmsg;
  err.link = "writeCoordData";
  err.type = INFO;
  err.ident = sccsid;
  em_show( &err );

  return(0);

}


GraphElement *get_comment_link_by_class( GraphElement *pcomment_vertex,
					 GraphClass class )
/****************************************************************************\
* Purpose: loop through the vertexes linked to the comment vertex
*	   until if finds a particular class
*
* Author:  Bill Rogers		Feb 1995
\****************************************************************************/
{
  GraphLink *llretrec;
  GraphElement *linkgraphrec;
  DB_STAT stat_link;

  stat_link = ll_first ( &graph_db, &pcomment_vertex, &llretrec,
					 GRAPH_VERTEX_LINK);

  while(stat_link == D_FOUND)
  {
    linkgraphrec = llretrec->graphnode;

    if( linkgraphrec->class == class )
	return(linkgraphrec);

    stat_link = ll_next( &llretrec, &llretrec );
  }

  return ( NULL );	/* no such class found */
}


int psfindfirstcoordbusrec( PSBusRecord **nextbusrec )
/****************************************************************************\
*       Function: findfirstcoordbusrec
*                 gets first bus record in pscor_db
*
*       Author: Bill Rogers     Oct 9, 1992
*
*       Called by:
*
*	nextbusrec: ptr returned for next bus rec
\****************************************************************************/
{
  PSBusRecord   *busvalrec;      /* ptr to key for bus search            */
  PSBusRecord   keyvalrecord;    /* storage for db_search key values     */
  PSBusRecord   *nextrec;        /* ptr assigned to address of next rec  */
  DB_STAT stat;

  /** if no base table exists just return (no data ) for list **/
  if(db_valid_db(&pscor_db) != D_OKAY) {
    printf(" *** WARNING *** pscor_db is empty\n" );
    return( 1 );
  }

  busvalrec = &keyvalrecord;	/* set the pointer to data field */
				/* (this is also the search key) */

  strncpy(busvalrec->key, "B", 1 );
  strncpy(busvalrec->name, "            ", 12 );
  stat = db_search( &pscor_db, &busvalrec, &nextrec, COORD_KEY_NAME1_BASE1);

  *nextbusrec = nextrec;
  if(stat == D_NOTFOUND) {
    if( 0 == strncmp(nextrec->key, busvalrec->key, sizeof(nextrec->key))) {
      stat = D_FOUND;  /* set to found -- positioned just before first bus */
    }
  }
  return(stat);

}


void display_coord_data( GraphElement *vertex )
/****************************************************************************\
* Purpose: General "routine" routine - display one or the other dialogs
*		Radio buttons don't know which display is up.
*          If GraphElement is NULL, then use static graphelement.
*	      otherwise use new
*
*	   DO NOT bring up dialog if graphelement param is null.
*
* Author: W.E. Rogers		
*
* Called by: set_graph_unit_and_origin_cb ( graphpscor.c )
*	     drag_rubberband	(toolmove.c)
*	     drawAreaButtonCB 	(toolbox.c)
*
\****************************************************************************/
{
static GraphElement *pactive_vertex;
  char *busname;

  if( coord_display_update == FALSE )  return;

/* debug */
/*  list_comment_strs(); */

  if( NULL == vertex )
  {
    if( NULL == pactive_vertex )
      return;
    else
      vertex = pactive_vertex;
  }

  if( vertex->class == GraphClassVertexBus ||
      vertex->class == GraphClassVertexName )
  {
    getCurBus( &busname );
    display_bus_coord_data( &busname[0] );
  }
  else  /* all other stuff....BendPoint, PaperVertex, DrawPoint, VertexFont,*/
	/* VertexComment, VertexGroup, etc.				    */
  {
    display_graph_coord_data( vertex );
  }

  pactive_vertex = vertex;
}

void activate_coord_update( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  coord_display_update= TRUE;
}

void deactivate_coord_update( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
{
  coord_display_update= FALSE;
}


void display_bus_coord_data( char *busname )
/****************************************************************************\
* Purpose: Put all bus data into the dialog
*
* Author: W.E. Rogers		
*
* Called by: drawAreaButtonCB ( toolbox.c )
*	     set_graph_unit_and_origin_cb ( graphpscor.c )
*
\****************************************************************************/
{

  char str[8];
  char pd_title[30];
  double xpos, ypos, ang;
  int x, y;
  GraphElement *pe;
  GraphElement *pname;
  GraphElement *pgraphobj;
  PSCoordFileRecord  *coordrec_p;	/* allocate 1 space for ptr  */

  DB_STAT stat;         /* status returned for general db access        */

  Vuit_MU( "M", "modify_bus_coord_dia");
  set_expose_occured();	   	/* set flag that CM button may be changed */
  Vuit_MU( "U", "graph_coord_data_form" );
  Vuit_MU( "U", "modify_graph_comment_form" );
  Vuit_MU( "M", "bus_coord_data_form" );

  enable_pushbutton ( "graph_unit_scaled_rb" );	      /* ascertain it wasn't */
  enable_pushbutton ( "graph_unit_unscaled_rb" );     /* turned off earlier! */

  lookup_and_fill_field( "full_name_text", busname, 0, 7 );
  lookup_and_fill_field( "nom_volts_text", busname, 8, 11 );

  /** make sure the coordinate file has been loaded **/
  if(db_valid_db(&pscor_db) != D_OKAY)
  {
    printf("INFO: pscor_db empty - Abort search of pscor_db\n");
    return;
  }
  pixel_flag = (int)XmToggleButtonGetState( widget_id("graph_units_pixel_rb") );

  coordrec_p = coord_db_bus_search( busname ); 
  stat = findBusGraphVertexRec   ( busname, &pe );

  if( D_FOUND == stat )
  {
    put_vtx_xy_values_in_text_box( pe, "bus_x_pos_text",
				       "bus_y_pos_text" );

    findVertxLinkedToVertx   ( &pname, pe, GraphClassVertexName );
    if( pname )
    {
      lookup_and_fill_field( "diag_name_text", pname->farvertex_id, 0, 7 );

      put_vtx_xy_values_in_text_box( pname, "name_x_pos_text",
					    "name_y_pos_text" );
    }

    if( pe->hide == HIDE )
      set_button_state( "hide_bus", True );
    else
      set_button_state( "show_bus", True );

    findVertxLinkedToVertx( &pgraphobj, pe, GraphClassVertexGenerator );

    if( pgraphobj != NULL )
    {
      if( pgraphobj->hide == HIDE )
      {
	ang = 0;
        set_button_state( "hide_gen", True );
      }
      else
      {
        ang = radiansBetweenPts( pe->x, pgraphobj->y,
			     pgraphobj->x, pe->y )/DegtoRad;
        set_button_state( "show_gen", True );
      }
    }
    else
    {
      ang = 0;
      set_button_state( "hide_gen", True );
    }

    sprintf( str, "%3.0f", ang);
    lookup_and_fill_field( "generator_angle_text", str, 0, 2 );

    findVertxLinkedToVertx( &pgraphobj, pe, GraphClassVertexReactor );

    if( pgraphobj != NULL )
    {
      if( pgraphobj->hide == HIDE )
      {
	ang = 0;
        set_button_state( "hide_reac", True );
      }
      else
      {
        ang = radiansBetweenPts( pe->x, pgraphobj->y,
			     pgraphobj->x, pe->y )/DegtoRad;
        set_button_state( "show_reac", True );
      }
    }
    else
    {
      ang = 0;
      set_button_state( "hide_reac", True );
    }

    sprintf( str, "%3.0f", ang);
    lookup_and_fill_field( "reactor_angle_text", str, 0, 2 );

    v_flag = (char)(pname->dply_seg +48);   /* convert 1-digit value to char */

    switch( v_flag )
    {
      case '0':
  	strcpy( pd_title, "Above Name" );
      break;

      case '1':
  	strcpy( pd_title, "Above Name" );
      break;

      case '2':
  	strcpy( pd_title, "Right of Name" );
      break;

      case ' ':
      case '3':
  	strcpy( pd_title, "Below Name" );
      break;

      case '4':
	strcpy( pd_title, "Left of Name" );
      break;

      case '5':
	strcpy( pd_title, "Do Not Print Voltage" );
      break;

      case '6':
	strcpy( pd_title, "Print voltage, without name" );
      break;

      default:
  	strcpy( pd_title, "Below Name" );
        /* printf("WARNING - bad value for v_flag\n"); */
    }

    cascade_pd_label_set( "voltage_location_op_menu", pd_title );

  }
  else		/* set default values */
  {
    lookup_and_fill_field( "diag_name_text", busname, 0, 7 );
    lookup_and_fill_field( "generator_angle_text", "  0", 0, 2 );
    lookup_and_fill_field( "reactor_angle_text", "  0", 0, 2 );
    set_button_state( "show_bus", True );
 
    v_flag = '3';
    cascade_pd_label_set( "voltage_location_op_menu", "Below Name");
    /* and now use the graph element vertex_id to default the
       abbreviated name to the bus name */
    
  }

  which_display = BUS_COORD_DIALOG;
}


void display_graph_coord_data( GraphElement *pvertex )
/****************************************************************************\
* Purpose: Put misc. graph data into the dialog
*
* Author: W.E. Rogers		
*
* Called by: drawAreaButtonCB ( toolbox.c )
*	     set_graph_unit_and_origin_cb ( graphpscor.c )
*
\****************************************************************************/
{
  Widget wid_id;
  char str[8];
  char *str_ptr;
  char text_str[256];
  DB_STAT stat_link;
  GraphLink *llretrec;
  GraphElement *linkgraphrec;
  GraphElement *pv1;
  GraphElement *pv2;
  char seg[5];
  GraphElement *bends[PSMAXBENDS + 2];
  int bendcnt;

  Vuit_MU( "M", "modify_bus_coord_dia");
  set_expose_occured();
  Vuit_MU( "U", "bus_coord_data_form" );
  Vuit_MU( "U", "modify_graph_comment_form" );
  Vuit_MU( "M", "graph_coord_data_form" );

  if( (wid_id = widget_id("graph_unit_scaled_rb")) )
  {
    if( (int)XmToggleButtonGetState( wid_id ) )
      lookup_and_fill_field( "coord_graph_scale_text", "(Unscaled)", 0, 9);
    else
      lookup_and_fill_field( "coord_graph_scale_text", "(Scaled)",   0, 7 );
  }

  enable_pushbutton ( "graph_unit_scaled_rb" );
  enable_pushbutton ( "graph_unit_unscaled_rb" );

  switch ( pvertex->class )
  {
    case GraphClassVertexBus:
    case GraphClassVertexName:
	/* not done in this routine - see display_bus_coord_data */
    break;

    case GraphClassVertexBendPoint:
      fill_coord_form( "BEND VERTEX", 'S' );
    break;

    case GraphClassVertexDrawPoint:
      fill_coord_form( "DRAW VERTEX", 'S' );
    break;

    case GraphClassPaperVertex:
      fill_coord_form( "PAPER CORNER", 'N' );
    break;

    case GraphClassVertexBorder:
      fill_coord_form( "BORDER CORNER", 'N' );
    break;

    case GraphClassVertexComment:
    case GraphClassVertexSubcomment:
    case GraphClassVertexOrgComment:
	/* done below */
    break;

    case GraphClassVertexLegendText:
      fill_coord_form( "LEGEND BOX", 'S' );
    break;

    case GraphClassPaperEdge:
      fill_coord_form( "PAPER EDGE", 'N' );
    break;

    case GraphClassVertexGenerator:
      fill_coord_form( "GENERATOR", 'S' );
    break;

    case GraphClassVertexReactor:
      fill_coord_form( "REACTOR", 'S' );
    break;

    case GraphClassEdgeSection:
      fill_coord_form( "TRANS LINE", 'S' );
    break;

    case GraphClassEdgeComplexSection:
      fill_coord_form( "EDGE COMPLX_SEC", 'S' );
    break;

    case GraphClassEdgeSubSection:
      fill_coord_form( "EDGE SUB_SECTION", 'S' );
    break;

    case GraphClassEdgeDraw:
      fill_coord_form( "DRAW LINE", 'S' );
    break;

    case GraphClassVertexLegendCorner:
      fill_coord_form( "LEGEND CORNER", 'S' );
    break;

    case GraphClassEdgeLegend:
      fill_coord_form( "LEGEND BORDER", 'S' );
    break;

    case GraphClassVertexLabelCorner:
      fill_coord_form( "LABEL CORNER", 'N' );
    break;

    case GraphClassEdgeLabel:
      fill_coord_form( "LABEL BORDER", 'N' );
    break;

    case GraphClassVertexLabelText:
      fill_coord_form( "LABEL DATA", 'N' );
    break;

    case GraphClassEdgeBorder:
      fill_coord_form( "PAPER BORDER", 'N' );
    break;

    case GraphClassVertexFont:
      fill_coord_form( "VERTEX FONT", 'N' );
    break;

    case GraphClassVertexGroup:
      fill_coord_form( "GROUP VERTEX", 'N' );
    break;

    default:
      lookup_and_fill_field( "coord_graph_vertex_class_text", "*UNKNOWN*",
				 0, 8 );
      printf("** WARNING ** from display_graph_coord_data (pscordat.c)\n" );
      printf("   No code to handle vertex class (%d)\n", pvertex->class );
    break;
  }

  pixel_flag = (int)XmToggleButtonGetState( widget_id("graph_units_pixel_rb") );

  lookup_and_fill_field( "graph_coord_cmplx_edge_bus1_txt", "", 0, 1 );
  lookup_and_fill_field( "graph_coord_cmplx_edge_bus2_txt", "", 0, 1 );
  lookup_and_fill_field( "gr_cor_cmplx_edge_dply_seg_txt",  "", 0, 1 );
  lookup_and_fill_field( "graph_cor_segment_txt",	    "", 0, 1 );

  switch ( pvertex->class )
  {
    case GraphClassVertexComment:
      Vuit_MU( "M", "modify_graph_comment_form" );
      Vuit_MU( "U", "graph_coord_data_form" );

      put_vtx_xy_values_in_text_box( pvertex, 
				     "modify_graph_x_text",
				     "modify_graph_y_text" );

      str_ptr = get_gg_string( pvertex );
      lookup_and_fill_field( "modify_graph_comment_str_text", str_ptr,
				 0, 35 );
      cmmt_ge = pvertex;

  /** now follow all vertex links of the comment record **/
      stat_link = ll_first ( &graph_db, &pvertex, &llretrec, GRAPH_VERTEX_LINK);

      text_str[0]='\0';
      lookup_and_fill_field( "modify_graph_font_text", " ",
				 0, 1);
      while(stat_link == D_FOUND)
      {
        linkgraphrec = llretrec->graphnode;
        switch( linkgraphrec->class )
        {
          case GraphClassVertexOrgComment:
  /*	  printf(" ORG Comment linked to this\n" ); */
            str_ptr = get_gg_string( linkgraphrec );
	    if( str_ptr != NULL )
            lookup_and_fill_field( "modify_graph_org_comment_text", str_ptr,
				 0, strlen(str_ptr)-1);
	  else
            lookup_and_fill_field( "modify_graph_org_comment_text", "NULL",
                                 0, 5 );
    	    org_cmmt_ge = linkgraphrec;
          break;

          case GraphClassVertexFont:
/*	    printf(" FONT Comment linked to this\n" ); */
            str_ptr = get_gg_string( linkgraphrec );
            lookup_and_fill_field( "modify_graph_font_text", str_ptr,
				 0, strlen(str_ptr)-1);
          break;

          default:
          printf("** WARNING ** UNKN vertex linked to this\n" );
        } 
        stat_link = ll_next( &llretrec, &llretrec );
      }
      which_display = COMMENT_DIALOG;
    break;

    case GraphClassEdgeSection:
    case GraphClassEdgeSubSection:
    case GraphClassEdgeComplexSection:
    case GraphClassEdgeDraw:
    case GraphClassPaperEdge:
    case GraphClassEdgeLegend:
    case GraphClassEdgeLabel:
    case GraphClassEdgeBorder:
	/* Note: To find vertexes of each end of edge */

      stat_link = findEdgeVertexes( pvertex, &pv1, &pv2 );

      put_vtx_xy_values_in_text_box( pv1, "coord_x_text_field",
					  "coord_y_text_field" );

      put_vtx_xy_values_in_text_box( pv2, "coord_x2_text_field",
					  "coord_y2_text_field" );

      if( GraphClassEdgeSection == pvertex->class )
      {
	int i;
	GraphElement *v1;
	GraphElement *v2;

        XtSetSensitive ( widget_id("gr_cor_line_seg_form"), True );
        edt_edge  = pvertex;	 /* for call to set_flow_data_segment */

		/* fill in line NAME */
	findComplexEdge( &cmplx_edge, pvertex );
        lookup_and_fill_field( "graph_coord_cmplx_edge_bus1_txt",
				 cmplx_edge->vertex_id, 0, 11 );
        lookup_and_fill_field( "graph_coord_cmplx_edge_bus2_txt",
				 cmplx_edge->farvertex_id, 0, 11 );

		/* show WHICH segment is flow data segement */
        sprintf( seg, "%d", cmplx_edge->dply_seg);
        lookup_and_fill_field( "gr_cor_cmplx_edge_dply_seg_txt", seg, 0,1  );

/*** Determine which segment this sub-edge is... ***/
					/* get list of sub-edges */
	bendcnt = createBendArray( bends, cmplx_edge, GraphTypeVertex );

					/* find sub-edge ends to look for */
	findEdgeVertexes( pvertex, &v1, &v2 );
	for ( i=1; i<=bendcnt; i++ )
	{
	  if( (v1 == bends[i-1] && v2 == bends[i]) ||
	      (v2 == bends[i-1] && v1 == bends[i]) )
	  {
	    sprintf( seg, "%d", i );
	    lookup_and_fill_field( "graph_cor_segment_txt", seg, 0,1  );
	  }
	}

/* Ensure RADIO buttons are set correctly - always do this LAST! */
/*		( text boxes NEED to be filled first )		 */
	set_flow_enabled = False;
        if( pvertex->dply_seg )
  	  set_button_state( "gr_cor_rbx_show_tb", True );
        else
  	  set_button_state( "gr_cor_rbx_no_show_tb", True );

	set_flow_enabled = True;
      }
      else
      {
        XtSetSensitive ( widget_id("gr_cor_line_seg_form"), False );
      }

      which_display = GENERAL_DIALOG;

    break;

    default:	/* non-comment data */
      put_vtx_xy_values_in_text_box( pvertex, "coord_x_text_field",
					  "coord_y_text_field" );

      lookup_and_fill_field( "coord_y2_text_field", "n/a", 0, 3 );
      lookup_and_fill_field( "coord_x2_text_field", "n/a", 0, 3 );

      which_display = GENERAL_DIALOG;

    break;
  }
}


void fill_coord_form( char *str, char scale )
{
int len_1, len_2;
char scale_str[15];
  
  if( scale=='S' || scale=='s' )
  {
    strcpy( scale_str, "(Scaled)" );
    len_2 = 9;
  }
  else
  {
    strcpy( scale_str, "Never Scaled" );
    len_2 = 13;
    set_button_state   ( "graph_unit_unscaled_rb",   True );
    disable_pushbutton ( "graph_unit_scaled_rb"   );
    disable_pushbutton ( "graph_unit_unscaled_rb" );
  }

  len_1 = strlen( str );

  lookup_and_fill_field( "coord_graph_vertex_class_text", str, 0, len_1 );
  lookup_and_fill_field( "coord_graph_scale_text",  scale_str, 0, len_2 );

}


void put_vtx_xy_values_in_text_box( GraphElement *ge, 
				    char *x_fld, char *y_fld )
/**************************************************************************\
* Purpose: Get Gadget/GraphElement X, Y Values
*	   ( Depends on which position the radio button is set )
*
* Author: Bill Rogers           8/13/96
*
* Called by:  display_graph_coord_data (4)
*             display_bus_coord_data   (2)
*
\**************************************************************************/
{
  Position x, y;

  if( XmToggleButtonGetState( widget_id("graph_item_graphelement_rb") ) )
  {
    x = ge->x;			/* Get the graphelement coordinates */
    y = ge->y;			/* (normally centered except letters */
  }
  else
  { 		/* Get GADGET's coordinates ( NW corner ) */
    XtVaGetValues( ge->wid, XmNx, &x, XmNy, &y, NULL );
  }

  if( (int)XmToggleButtonGetState( widget_id("graph_origin_abs_rb" ) ))
  {
    put_coord_value_in_text_box( x_fld, x );
    put_coord_value_in_text_box( y_fld, y );
  }
  else
  {
    put_coord_value_in_text_box( x_fld, MOTIF_to_ps_x( x, "unkn", "", ""));
    put_coord_value_in_text_box( y_fld, MOTIF_to_ps_y( y, "unkn", "", ""));
  }

}


void put_coord_value_in_text_box( char *field_name, double x )
/**************************************************************************\
\**************************************************************************/
{
  char str[8];

    if(XmToggleButtonGetState( widget_id ( "graph_origin_abs_rb" )))
    {
      x += SCRN_BORDER;
    }

    if( pixel_flag )
      sprintf( str, "%6.0f", x );
    else
      sprintf( str, "%6.2f", x );
    lookup_and_fill_field( field_name, str, 0, 7 );
}
 
void get_gg_xy_values( GraphElement *ge, int *x, int *y )
/****************************************************************************\
* Purpose: Get Gadget/GraphElement X, Y Values
*	   ( Depends on which position the radio button is set )
*
* Author: Bill Rogers           1/19/95
*
* Called by:  display_graph_coord_data
*             display_bus_coord_data
*
\****************************************************************************/
{
  Position xx, yy;

  if( XmToggleButtonGetState( widget_id("graph_item_graphelement_rb") ) )
  {
    *x = ge->x;			/* Get the graphelement coordinates */
    *y = ge->y;			/* (normally centered except letters */
  }
  else
  {		/* Get GADGET's coordinates ( NW corner ) */
    XtVaGetValues( ge->wid, XmNx, &xx, XmNy, &yy, NULL );
    *x = xx;
    *y = yy;
  }
}


char *get_gg_string( GraphElement *ge )
/****************************************************************************\
* Purpose: Get Gadget label string
*
* Author: Bill Rogers           2/21/95
*
* Called by:  display_graph_coord_data
*
\****************************************************************************/
{
  XmString xstr;
  char *pcmt_str;

  XtVaGetValues( ge->wid, XmNlabelString, &xstr, NULL );
  pcmt_str = cstring_to_string(xstr);

#ifdef WWWX
  XmStringFree(xstr);	/* Don't do this - it kills the prts  WHY?? */
#endif

  return ( pcmt_str );
}



int gdgt_ge_x_offset ( GraphElement *ge )
/**************************************************************************\
* Purpose: Returns OFFICIAL offsets between gadget and graphelement
	i.e. converts coordinates to MOTIF coordinates.
*
\**************************************************************************/
{
#define CHAR_WD	 5	/* this must match stuff in EdgeGP.h */
#define CHAR_HT	 7
#define H_CHR_SP 1
#define V_CHR_SP 2
#define MRGN 1
#define SHOW_NAME_ABBR 1L<<0
#define SHOW_NAME_FULL_NOKV 1L<<1
#define SHOW_NAME_FULL 1L<<2

  char *text;
  XmString x_str;
  int str_wd, junk;
  int name_opt;
  int size;

  switch( ge->class )
  {
    case GraphClassVertexBus:
    case GraphClassVertexGenerator:
      if( ge->hide == NO_HIDE )
        return( icon_bus_width );
      else
        return( HIDDEN_BUS_RADIUS );
    break;

    case GraphClassVertexName:
      v_flag = (char)(ge->dply_seg+48);
      if( XmToggleButtonGetState( widget_id("view_menu_solution_pd")))
      {
        switch( v_flag )
        {
          case '4':
	    XtVaGetValues( ge->wid, XmNlabelString, &x_str, NULL );
	    XmStringGetLtoR( x_str, XmSTRING_DEFAULT_CHARSET, &text);
	    get_box_size_of_string( text, &str_wd, &junk );
	    get_bus_name_solution_opt(&name_opt);
	    switch( name_opt )
	    {
	      case SHOW_NAME_ABBR:
	      case SHOW_NAME_FULL_NOKV:
		size = 8;
	      break;

	      case SHOW_NAME_FULL:
		size = 12;
	      break;
	    }

if( str_wd <size )
{
 printf("ERROR - string too short!\n");
 str_wd = size;
}
            return(((CHAR_WD + H_CHR_SP)*(str_wd-size) + MRGN )*zoom_factor );
	  break;

	  case '0':
	  case '1':
	  case '2':
	  case '3':
	  case '5':
	  case '6':
	  case ' ':
	  break;

	}
      }
      return( MRGN * zoom_factor );
    break;

    case GraphClassVertexReactor:
      return( REACT_RADIUS );
    break;

    case GraphClassVertexBendPoint:
      return( PT_RADIUS );
    break;

    case GraphClassVertexDrawPoint:
    case GraphClassPaperVertex:
    case GraphClassVertexBorder:
    case GraphClassVertexLabelCorner:
    case GraphClassVertexLegendCorner:
      return( PT1_RADIUS );
    break;

    default:
      return ( 0 );

  }
}


int gdgt_ge_y_offset ( GraphElement *ge )
/****************** better method of adjustment ********************
*  Dimension text_ht, border_wd, shadow_wd, shadow_wd, hilite_wd, y;
*  XtVaGetValues( wid, XmNheight, &text_ht,
*		      XmNborderWidth, &border_wd,
*		      XmNshadowThickness, &shadow_wd,
*		      XmNhighlightThickness, &hilite_wd,
*		      XmNy, &y, NULL ):
\*************************************************************************/
{
  XmString x_str;
  char *str;
  char v_flag;
  int text_ht, junk;

  switch( ge->class )
  {
    case GraphClassVertexBus:
    case GraphClassVertexGenerator:
      if( ge->hide == NO_HIDE )
        return( icon_bus_height );
      else
        return( HIDDEN_BUS_RADIUS );
    break;

    case GraphClassVertexName:				 /* needs work? */
      v_flag = (char)(ge->dply_seg+48);
      if( v_flag == '1' &&
		 XmToggleButtonGetState( widget_id("view_menu_solution_pd")) )
      {

	XtVaGetValues( ge->wid, XmNlabelString, &x_str, NULL );
	text_ht = XmStringLineCount(x_str);
        return(((CHAR_HT+V_CHR_SP)*text_ht -1 -V_CHR_SP +MRGN)*zoom_factor );
      }
      return( (CHAR_HT -1 +MRGN)*zoom_factor );
    break;

    case GraphClassVertexComment:
    case GraphClassVertexSubcomment:
    case GraphClassVertexLegendText:
    case GraphClassVertexLabelText:
      return( (CHAR_HT -1 +MRGN)*zoom_factor );
		/* Note: Unlike Bus, Gen, Bendpt & Drawpt, Name & comments
		   widgets change size during zoomin and zoomout */
    break;

    case GraphClassVertexBendPoint:
      return( PT_RADIUS );
    break;
 
    case GraphClassVertexDrawPoint:
    case GraphClassPaperVertex:
    case GraphClassVertexBorder:
    case GraphClassVertexLabelCorner:
    case GraphClassVertexLegendCorner:
      return( PT1_RADIUS );
    break;
 
    default:
      return( 0 );
  }
}



void set_flow_data_segment( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/**************************************************************************\
* Purpose:  Answers the gr_cor_rbx_show_tb toggle button
*	    if set to "ON" then change all required items to show
*	    correct segment of display
*	NOTE:  Turning "OFF" an "ON" segment should set a -1 in flow seg
*
* Author: Wm E. Rogers
*
\***************************************************************************/
{
  DB_STAT stat;
  GraphElement update_rec;
  GraphElement *bends[PSMAXBENDS + 2];
  GraphElement *old_bend;
  char seg[5];
  int cmplx_dply;
  int edge_dply;
  int old;

  Boolean update_needed = False;
  GraphElement *pupdate_rec = &update_rec;


  if( set_flow_enabled )      /* excutes only if USERS push the radio button */
  {
    if((int)XmToggleButtonGetState(widget_id("gr_cor_rbx_show_tb") ) )
    {
      edge_dply = 1;
      lookup_and_get_field( "graph_cor_segment_txt" , seg, 2 );
      cmplx_dply = atoi_cnt_zero(seg, 2);
      lookup_and_fill_field( "gr_cor_cmplx_edge_dply_seg_txt", seg, 0,1  );
    }
    else
    {		/* change to NO FLOW status */
      edge_dply  = 0;
      cmplx_dply =-1;
    }
  
/******************** update the old sub edge (if any) ***********************/
    old = cmplx_edge->dply_seg;
    if( old > 0 )
    {
      createBendArray( bends, cmplx_edge, GraphTypeEdge );
	old_bend = bends[old-1];

	memcpy( pupdate_rec, bends[old-1], sizeof(GraphElement));
	pupdate_rec->dply_seg = 0;
	stat = db_update(&graph_db, &bends[old-1], &pupdate_rec);

	XtVaSetValues( pupdate_rec->wid, XiNsymbolSegment, 0, NULL );
    }

/**************** set the new status seg of cmplx edge ********************/
    sprintf( seg, "%d", cmplx_dply);
    lookup_and_fill_field( "gr_cor_cmplx_edge_dply_seg_txt", seg, 0,1  );
  
	  /* update the complex edge */
    memcpy( pupdate_rec, cmplx_edge, sizeof(GraphElement));
    pupdate_rec->dply_seg = cmplx_dply;
    stat = db_update(&graph_db, &cmplx_edge, &pupdate_rec);
  
/****************** update the sub edge ***********************************/
    memcpy( pupdate_rec, edt_edge, sizeof(GraphElement));
    pupdate_rec->dply_seg = edge_dply;
    stat = db_update(&graph_db, &edt_edge, &pupdate_rec);

    XtVaSetValues( edt_edge->wid, XiNsymbolSegment, 1, NULL );

	/* immediatelly show the changes */
    if( (int)XmToggleButtonGetState(widget_id("view_menu_solution_pd")) )
    {
      char  outbuf_array[BUFSIZE];
      char  inbuf_array [BUFSIZE];
      char *pfcmdstr = outbuf_array;
      char *pfdataret = inbuf_array;
      char pout[256];
      XmString x_str;
 
      if( old>0 )
      {
        sprintf(pout, " ");
        x_str =  XmStringCreateLtoR(pout, XmSTRING_DEFAULT_CHARSET);

       XtVaSetValues( old_bend->wid,
        XiNlabelString, x_str,
        XiNlabelOn, (Boolean)FALSE,
        NULL);
        XmStringFree(x_str);
      }

      sprintf(pfcmdstr, "/get_data,type=output\n");
      sprintf(pout, "L     %12.12s %12.12s*  \n", cmplx_edge->vertex_id,							  cmplx_edge->farvertex_id );
      printf("UPDATE edge %s\n", pout);
      strcat(pfcmdstr, pout);
      strcat(pfcmdstr, ENDOFMSG);

      ipc_synch_rw(pfdataret, pfcmdstr);

      process_update_data(pfdataret);

    }
  }
}



PSCoordFileRecord *coord_db_bus_search( char *busname )
/****************************************************************************\
*	Standardized search for bus record
*
*	Bill Rogers	9/24/93
\****************************************************************************/
{

  PSCoordFileRecord  key;		/* allocate space for key string */
  PSCoordFileRecord  *key_p;		/* allocate space for key ptr	*/

  PSCoordFileRecord  *coordrec_p;	/* allocate 1 space for ptr  */

  DB_STAT stat;         /* status returned for general db access        */

  key_p = &key;      /*  set ptr to memory location
					   holding search key string */

  if(db_valid_db(&pscor_db) != D_OKAY)
  {
    printf("INFO: pscor_db empty - do not search pscor_db\n");
    return (NULL);
  }
  strncpy ( key.bus.key, "B", 1 );
  strncpy ( key.bus.name, busname, 12 );  

  stat = db_search ( &pscor_db,
		     &key_p,
		     &coordrec_p,
		     COORD_KEY_NAME1_BASE1 );

  if(stat != D_FOUND){
        /* dlc: remove print statement occurs during normal processing */
	/* printf("Bus not found \n"); */ 
	return( NULL );
  }

  return( coordrec_p );

}


PSCoordFileRecord *coord_db_branch_search( char *busname1, char *busname2 )
/****************************************************************************\
*	Standardized search for bus record
*
*	Bill Rogers	9/24/93
\****************************************************************************/
{

  PSCoordFileRecord  key;		/* allocate space for key string */
  PSCoordFileRecord  *key_p;		/* allocate space for key ptr	*/
  PSCoordFileRecord  *coordrec_p;	/* allocate 1 space for ptr  */
  DB_STAT stat; /* status returned for general db access        */

  key_p = &key; /*  set ptr to memory location holding search key string */

  if(db_valid_db(&pscor_db) != D_OKAY)
  {
    printf("INFO: pscor_db empty - do not search pscor_db\n");
    return (NULL);
  }
  memset(&key.branch.idx, '\0', sizeof(key) );
  strncpy ( key.branch.name1, busname1, 12 );  
  strncpy ( key.branch.name2, busname2, 12 );  

  stat = db_search ( &pscor_db,
		     &key_p,
		     &coordrec_p,
		     COORD_NAME1_BASE1_NAME2_BASE2 );

  if(stat == D_FOUND){
     return( coordrec_p );
  }
  memset(&key.branch.idx, '\0', sizeof(key) );
  strncpy ( key.branch.name1, busname2, 12 );  
  strncpy ( key.branch.name2, busname1, 12 );  

  stat = db_search ( &pscor_db,
		     &key_p,
		     &coordrec_p,
		     COORD_NAME1_BASE1_NAME2_BASE2 );

  if(stat == D_FOUND){
     return( coordrec_p );
  }

  return( NULL );
}


void write_coord_plot_dlg_to_db( Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs)
/************************************************************************\
*  Purpose: writes all dialog data to coord_db
*
*  Author: Bill Rogers		Sept 29 1993
*  Called by: APPLY pushbutton on modify_bus_coord_dia window
\************************************************************************/
{
  char busname[15];
  char base[6];
  char angle[6];
  PSCoordFileRecord new_rec;
  PSCoordFileRecord *p_new_rec = &new_rec;

  PSCoordFileRecord *p_rtn_rec; 
  PSCoordFileRecord *coordrec_p;	/* allocate 1 space for ptr  */
  GraphElement *pgraphbus;
  GraphElement *pgraphname;
  GraphElement graphupdate;
  GraphElement *pgraphupdate = &graphupdate;
  char *namebase;
  char pout[15];
  DB_STAT stat;
  XmString strx;

  char comment_str[256];

  switch ( which_display )
  {
    case BUS_COORD_DIALOG:

    getCurBus( &namebase );
    coordrec_p = coord_db_bus_search( namebase );

    if( NULL == coordrec_p )	/* if curbus is not in coord_db..... */
    {
      memset ( &new_rec, ' ', sizeof( PSCoordFileRecord) );
      new_rec.bus.eol[0] = '\0';
      new_rec.bus.idx = (long)card_index++;
      strncpy( new_rec.bus.key, "B",	     sizeof( new_rec.bus.key        ) );
      strncpy( new_rec.bus.display, " ",     sizeof( new_rec.bus.display    ) );
      strncpy( new_rec.bus.name  , namebase, sizeof( new_rec.bus.name	    ) +
					     sizeof( new_rec.bus.base	    ) );
      strncpy( new_rec.bus.abbr  , namebase, sizeof( new_rec.bus.abbr       ) );
      strncpy( new_rec.bus.v_flag     ,  "3",sizeof( new_rec.bus.v_flag     ) );
      strncpy( new_rec.bus.generator_ang,"  0",
					     sizeof(new_rec.bus.generator_ang));
      strncpy( new_rec.bus.reactor_ang  ,"  0",
					     sizeof(new_rec.bus.reactor_ang  ));

      p_new_rec = &new_rec;
      stat = db_insert_opt( &pscor_db, &p_new_rec, &p_rtn_rec, optsv);

      if(D_OKAY != stat)
        printf("Programmer Error: no coord changes saved for %s\n", namebase);
 
    /* test confirm its in  - remove to get better performance */
      coordrec_p = coord_db_bus_search( namebase );
      if(coordrec_p == NULL) {
        printf("Programmer Error - new coord record not found\n");
        return;
      }
      coordrec_p = p_rtn_rec;
    }
    p_new_rec = &new_rec;
    memcpy(p_new_rec,coordrec_p, sizeof(PSCoordFileRecord));
    
    lookup_and_get_field( "diag_name_text" , busname, 9 );
    add_blanks_if_short ( busname, 9, "diag_name_text" );
    strncpy( new_rec.bus.abbr, busname, 9 );

    stat = findBusGraphVertexRec( namebase, &pgraphbus );
    findVertxLinkedToVertx( &pgraphname, pgraphbus, GraphClassVertexName );

    pgraphupdate = &graphupdate;
    memcpy ( pgraphupdate, pgraphname, sizeof(GraphElement));
    strncpy( pgraphupdate->farvertex_id, busname,
					sizeof(pgraphname->farvertex_id) );
    stat = db_update(&graph_db, &pgraphname, &pgraphupdate );

    lookup_and_get_field( "generator_angle_text" , angle, 4 );
    add_blanks_if_short( angle, 4, "generator_angle_text" );
    reformat (angle, angle, "f3.0");
    strncpy( new_rec.bus.generator_ang, angle, 4 );

    lookup_and_get_field( "reactor_angle_text" , angle, 4 );
    add_blanks_if_short( angle, 4, "reactor_angle_text" );
    reformat (angle, angle, "f3.0");
    strncpy( new_rec.bus.reactor_ang, angle, 4 );

    if( option_setting( "show_bus" ) )
      new_rec.bus.display[0] = ' ';
    else
      new_rec.bus.display[0] = '1';

    new_rec.bus.v_flag[0] = v_flag;

    stat = db_update( &pscor_db, &coordrec_p, &p_new_rec );
    if(D_OKAY != stat) {
      printf("Programmer Error: coord changes not updated for %s\n", namebase);
    };
   break;

   case COMMENT_DIALOG:

     lookup_and_get_field( "modify_graph_org_comment_text" , comment_str, 75 );
     add_blanks_if_short( comment_str, 75, "comment_gadget_string" );

     strx = XmStringCreateLtoR( comment_str, XmSTRING_DEFAULT_CHARSET );
     XtVaSetValues( org_cmmt_ge->wid, XmNlabelString, strx, NULL );
     XtVaSetValues( cmmt_ge->wid,     XmNlabelString, strx, NULL );
     XmStringFree( strx );
  
     send_comments_and_defines_to_pf();
     refresh_comments(); 
   break;

   case GENERAL_DIALOG:
   default:
     printf("WARNING from write_coord_plot_dlg_to_db.\n" );
     printf("        Cannot update anything from this coord dialog\n" );
  }

  coord_display_update= FALSE;	/* prevent dialog from re-appearing on */
				/* other toolbox commands such as MOVE */
}


void modifyBusLocation( Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs )
/****************************************************************************\
* Purpose: Read all modify_bus_coord_dia data and update 
*	   any changed gadget stuff
*
* Called by: buttonActivation of APPLY on modify_bus_coord_dia dialog.
\****************************************************************************/
{
  GraphElement *pgraphbus;
  GraphElement *pvtxname;
  DB_STAT stat;

  char *namebase;
  char str[16];
  int x, y;

  getCurBus( &namebase );
  stat = findBusGraphVertexRec( namebase, &pgraphbus );
  findVertxLinkedToVertx( &pvtxname, pgraphbus, GraphClassVertexName );

  if( D_FOUND == stat )
  {

    lookup_and_get_field( "bus_x_pos_text", str, 6 );
    x = pscoord_to_MOTIF_x( str, 6, pgraphbus->vertex_id );
    lookup_and_get_field( "bus_y_pos_text", str, 6 );
    y = pscoord_to_MOTIF_y( str, 6, pgraphbus->vertex_id );

    update_location_of_ge_and_attachments( pgraphbus, x-pgraphbus->x,
						      y-pgraphbus->y );

    lookup_and_get_field( "name_x_pos_text", str, 6 );
    x = pscoord_to_MOTIF_x( str, 6, pgraphbus->vertex_id );
    lookup_and_get_field( "name_y_pos_text", str, 6 );
    y = pscoord_to_MOTIF_y( str, 6, pgraphbus->vertex_id );

    if( pvtxname->x != x || pvtxname->y != y )
        update_location_of_ge_and_attachments( pvtxname, x-pvtxname->x,
					 		 y-pvtxname->y );
  }

  adjust_bus_angle( pgraphbus, "generator_angle_text", "show_gen",
					GraphClassVertexGenerator, G_DIST );
  adjust_bus_angle( pgraphbus, "reactor_angle_text", "show_reac",
					GraphClassVertexReactor, R_DIST );

}

void adjust_bus_angle( GraphElement *pgraphbus, char *angle_text,
			 char *show_box, GraphClass class, int dist )
/**************************************************************************\
* Purpose: Move gen or reac symbol to specific angle location
*
* Called by: above
\**************************************************************************/
{
  GraphElement *pgraphobj;
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphLink *lnk; 
  DB_STAT stat;
  double c_ang, d_ang;
  char str[16];
  char new_hide_status, old_hide_status;

  old_hide_status = pgraphnode->hide;

  lookup_and_get_field( angle_text, str, 4 );
  c_ang = atof_cnt_zero( str, 3 );       /* convert to float */

  if ( XmToggleButtonGetState(widget_id(show_box)))
    new_hide_status = NO_HIDE;
  else
    new_hide_status = HIDE;

  findVertxLinkedToVertx( &pgraphobj, pgraphbus, class );

  if( pgraphobj )
  {
    d_ang = radiansBetweenPts( pgraphobj->x, pgraphobj->y,
				pgraphbus->x, pgraphbus->y )/DegtoRad;
                        /* NOTE: y is reverse due to MOTIF reversed coord */
 
    if( c_ang != d_ang || pgraphobj->hide != new_hide_status )
    {
      memcpy(pgraphnode, pgraphobj, sizeof(GraphElement));
      pgraphnode->x = pgraphbus->x+(int)(cos(c_ang*DegtoRad)*dist*zoom_factor);
      pgraphnode->y = pgraphbus->y-(int)(sin(c_ang*DegtoRad)*dist*zoom_factor);
      pgraphnode->hide = new_hide_status;

      db_update(&graph_db, &pgraphobj, &pgraphnode);

      position_gadget_wrt( pgraphnode );   /* update gen/reac location */

      stat = ll_first(&graph_db, &pgraphobj, &lnk, GRAPH_EDGE_LINK);
      if (stat == D_FOUND)
      {
        if(lnk->graphnode->display == GraphDisplayOn)
        {
          updateEdge(lnk->graphnode);
        }
      }

      if( new_hide_status != old_hide_status )
      {
	switch (new_hide_status)
	{
	  case HIDE:
	    turnOffGenReac( pgraphbus, class );
	  break;

	  case NO_HIDE:
	    manageVertex(pgraphobj);	/* gen vertex */
            findGraphEdgebyName2( pgraphbus->vertex_id,
					 "ZZGenerator ", &pgraphobj );
	    manageEdge(pgraphobj);
	  break;
	}
      }
    }
  }
  else
  {
    printf("ERROR (pscordat.c) - missing gen/reac gagdet.\n" );
  }
}

void turnOffGenReac( GraphElement *pgraphbus, int class )
{
  GraphElement *pgraphobj;
  char name[13];

  if ( class==GraphClassVertexGenerator )
     strcpy( name, "ZZGenerator " );
  else if ( class==GraphClassVertexReactor )
     strcpy( name, "ZZReactor   " );
  else
  {
     printf("ERROR - in turnOffGenReac (pscordat.c) - incorrect class\n");
     return;
  }

  findVertxLinkedToVertx( &pgraphobj, pgraphbus, class );
  turnoffGraphElement(pgraphobj);

  findGraphEdgebyName2( pgraphbus->vertex_id, name, &pgraphobj );
  turnoffGraphElement( pgraphobj );
}


int update_bus(char *psolnData, char *userdata )
/***********************************************************************\
* Purpose: Used to process only the single pf bus data.
*
* Called by: set_volts_relative_of_name
\***********************************************************************/
{

  if(isbus((pfOutRecord *)psolnData))
  {
    refreshBusImage( (pfOutRecord *)psolnData );
  }

  return(1);
}



void set_volts_relative_of_name (Widget w, XtPointer tag,
                                 XmAnyCallbackStruct *cbs )
/****************************************************************************\
*
*
* Called by: buttonActivation of togglebuttons on modify_bus_coord_dia dialog.
\****************************************************************************/
{
  GraphElement *pvtxname;
  GraphElement *pgraphvertex;
  GraphElement updaterec;
  GraphElement *pupdaterec = &updaterec;
  char *namebase;
  DB_STAT stat;
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;

  char newkey[256];
  int cnt;


  getCurBus( &namebase );
  stat = findBusGraphVertexRec( namebase, &pgraphvertex );
  findVertxLinkedToVertx( &pvtxname, pgraphvertex, GraphClassVertexName );

	/* update GraphElement */
  memcpy( pupdaterec,  pvtxname, sizeof( GraphElement) );
  pupdaterec->dply_seg = atoi_cnt( &v_flag, 1 );
  db_update( &graph_db, &pvtxname, &pupdaterec );

  sprintf(pfcmdstr, "/get_data,type=output\n");
  sprintf(newkey, "B     %12.12s   \n", pgraphvertex->vertex_id);
  strcat(pfcmdstr, newkey);
  strcat(pfcmdstr, ENDOFMSG);
  ipc_synch_rw(pfdataret, pfcmdstr);

  cnt = forEachipfLine(pfdataret, update_bus, NULL);


/*  printf("Voltage flag is (%c)\n", v_flag ); */

}
void set_flag( Widget w, XtPointer tag, XmAnyCallbackStruct *cbs )
/****************************************************************************\
*
*
* Called by: buttonActivation of togglebuttons on modify_bus_coord_dia dialog.
\****************************************************************************/
{
  char *pctag;
  pctag = (char *)tag;
  v_flag = pctag[0];
}



int psprintCoordData(Widget widget, XtPointer tag, XmAnyCallbackStruct *cbs)
/*****************************************************************************\
* Purpose: debug
* Function: printCoordData 
*   print the coordinate file bus and branch data to the screen
*	Author: Dan Clark	Dec, 1991	Modified: Dec 17, 1991
\*****************************************************************************/
{
  PSCoordFileRecord keyvalrecord;	/* storage for db_search key values */
				/* used for any search */
  PSCoordFileRecord *keyvalrec;	/* ptr to key values in db_search  */
				/* or ptr to current record in db_next */
				/* similar to a cursor in sql */
  PSCoordFileRecord *nextrec;	/* ptr assigned to address of next rec */
				/* used by db_search & db_next */
  PSCoordFileRecord *branchvalrec;/* ptr to key for branch search */
  PSCoordFileRecord *nextbranchrec;/* ptr returned for next branch rec */
  PSCoordFileRecord *busvalrec;	/* ptr to key for bus search */
  PSCoordFileRecord *nextbusrec;	/* ptr returned for next bus rec */

  DB_STAT stat;		/* status returned for general db access */
  DB_STAT statbus;	/* status returned for bus queries of db */
  DB_STAT statbranch;	/* status returned for branch queries of db */

  /** retrieve the first Bus record **/
/*** test use of combination key
  busvalrec = &keyvalrecord;
  strncpy(busvalrec->key, "B", 1);
  statbus = db_search(&pscor_db,&busvalrec, &nextbusrec, COORD_KEY);
  if(statbus != D_FOUND)
  { 
     printf("Warning: no busses found in table\n");
  }
***/
  busvalrec = &keyvalrecord;
  strncpy(busvalrec->bus.key, "B", 1);
  strncpy(busvalrec->bus.name, "            ", 12);
  statbus = db_search(&pscor_db, &busvalrec, &nextbusrec, COORD_KEY_NAME1_BASE1);
  if(statbus != D_FOUND)
  { 
     printf("Note: positioned before first bus\n");
     statbus = D_FOUND;
  }
  if(nextbusrec->bus.key[0] != 'B')
  {
     printf("Note: search did not find bus record\n");
  }

  /** write each bus record followed by all the associated branch records **/
  while(statbus == D_FOUND)
  {
     printf("bus: %s\n", nextbusrec->bus.name );

    /** retrieve the first branch Record for this bus **/
    branchvalrec = &keyvalrecord;
    strncpy(branchvalrec->branch.key, " ", 1);
    strncpy(branchvalrec->branch.name1, nextbusrec->bus.name , 12);
    statbranch = db_search(&pscor_db,&branchvalrec,&nextbranchrec,COORD_NAME1_BASE1);
    if(statbranch != D_FOUND)
    { 
      printf("Note: no branches found in table for bus\n");
    }

    /** write each branch record for bus **/
    while( statbranch == D_FOUND)
    {
      if(nextbranchrec->branch.key[0] == 'B')
      {
        printf("Note: skipping write of Bus record \n");
      } else {
        printf("branch: %s\n", nextbranchrec);
      }

      /* get the next branch for this bus **/
      branchvalrec = nextbranchrec;
      statbranch = db_keynext(&pscor_db, &branchvalrec, &nextbranchrec, COORD_NAME1_BASE1);
    } /* end branch loop */

    /** retrieve the first farend branch Record for this bus **/
    branchvalrec = &keyvalrecord;
    strncpy(branchvalrec->branch.key, " ", 1);
    strncpy(branchvalrec->branch.name1, "            " , 12);
    strncpy(branchvalrec->branch.name2, nextbusrec->bus.name , 12);
    statbranch = db_search(&pscor_db,&branchvalrec,&nextbranchrec,COORD_NAME2_BASE2);
    if(statbranch != D_FOUND)
    { 
      printf("Note: no farend branches found in table for bus\n");
    }

    /** write each farend branch record for bus **/
    while( statbranch == D_FOUND)
    {
      if(nextbranchrec->branch.key[0] == 'B')
      {
        printf("Note: skipping write of farend Bus record \n");
      } else {
        printf("farbranch: %s\n", nextbranchrec);
      }

      /* get the next branch for this bus **/
      branchvalrec = nextbranchrec;
      statbranch = db_keynext(&pscor_db, &branchvalrec, &nextbranchrec, COORD_NAME2_BASE2);
    } /* end branch loop */

    /** get the next bus **/
    busvalrec = nextbusrec;
    statbus = db_next(&pscor_db, &busvalrec, &nextbusrec,COORD_KEY_NAME1_BASE1);
    if(nextbusrec->bus.key[0] != 'B')
    {
       printf("Note: forcing end - next ran past bus recs\n");
       printf("last %s\n", nextbusrec);
       statbus = D_NOTFOUND;
    }
  } /* end bus loop */

  return(0);

}


int psprintcoord_db( char type )
/****************************************************************************\
*  Function: prints out specific type of coordinate records.
*		(a DEBUG function)
*
*   Author: Bill Rogers     Oct 3, 1994
*
*    Called by: (formly used to check the contents by pscreateCommentVertexes)
*
\****************************************************************************/
{

  PSBusRecord   *busvalrec;      /* ptr to key for bus search            */
  PSBusRecord   keyvalrecord;    /* storage for db_search key values     */
  PSBusRecord   *nextrec;        /* ptr assigned to address of next rec  */
  DB_STAT stat;

  /** if no base table exists just return (no data ) for list **/
  if(db_valid_db(&pscor_db) != D_OKAY) {
    printf(" *** WARNING *** pscor_db is empty\n" );
    return( 1 );
  }

  busvalrec = &keyvalrecord;	/* set the pointer to data field */
				/* (this is also the search key) */

  printf("Dumping coord_db.....\n");

/*  strncpy(busvalrec->key, type, sizeof(busvalrec->key) ); */

  stat = db_first( &pscor_db, &nextrec, COORD_IDX );

  while(stat == D_FOUND)
  {
    printf("(%s)\n", &nextrec->key[0] );
 
    busvalrec = nextrec;
    stat = db_next( &pscor_db, &busvalrec, &nextrec, COORD_IDX );
/*    if(strncmp(busvalrec->cor.key,nextrec->cor.key, sizeof(nextrec->cor.key)) == 0) {
*      break;
*    }
*/
  }
  printf("************ END DUMP ****************\n");

  return(stat);

}

 

Boolean bad_coord_sb( Widget *id, Widget *list_id, Widget *text_id )
/****************************************************************** 
* purpose: debug
*	 find the widget id of the coordinate edit selection list
*	 and the sub-widget ids of the list and text boxes.
*	 ( Not Used )
******************************************************************* */
{
  /**  find widget id of selection box **/
  if((*id = widget_id("cor_edit_selection")) == FALSE) return(True);

  *list_id = (Widget)XmSelectionBoxGetChild( *id, XmDIALOG_LIST);
  if(*list_id == NULL) {
    printf("PROGRAMMER ERROR no XmDIALOG_LIST for cor_edit_selection\n");
    return(True);
  }

  *text_id = (Widget)XmSelectionBoxGetChild( *id, XmDIALOG_TEXT);
  if( *text_id == NULL) {
    printf("PROGRAMMER ERROR no XmDIALOG_TEXT for cor_edit_selection\n");
    return(True);
  }
  return(False);
}

void cor_selection_edit( Widget widget, char *tag,
				XmSelectionBoxCallbackStruct *cb)
/*****************************************************************************\
* Purpose: debug only
* Replace the list item with the new string 
* Called by: OK pushbutton on coord selection box
* Not Used
\*****************************************************************************/
{
  Widget id;
  Widget list_id;
  Widget text_id;
  char *stmp;
  int *position_list;
  int position_count;

  if( bad_coord_sb( &id, &list_id, &text_id ) ) return;

  XmTextShowPosition(text_id,(XmTextPosition)0);
  
#ifdef DEBUG
    stmp = cstring_to_string(cb->value);
    printf("new: %s\n", stmp);
#endif

  XmListGetSelectedPos(list_id, &position_list, &position_count);
  if(position_count <= 0) {
    printf("error - no postion selected\n");
    return;
  }

  XmListReplaceItemsPos(list_id, &cb->value, (int)1, position_list[0]);

  return;
}


/*****************************************************************************\
* Purpose: debug
* Fill an edit list with the coordinate data.
* Not Used
\*****************************************************************************/
void fill_cor_edit_selection( Widget widget, char *tag, char *callback_data)
{
  Widget id;
  Widget list_id;
  Widget text_id;
  PSCoordFileRecord keyvalrecord;
  PSCoordFileRecord *nextptr;
  PSCoordFileRecord *searchkey = &keyvalrecord;
  DB_STAT stat;
  int cnt = 0;
#define MAXLINES 1024
  XmString xlns[MAXLINES];

  if( bad_coord_sb( &id, &list_id, &text_id ) ) return;

  XtSetSensitive((Widget)XmSelectionBoxGetChild(id, XmDIALOG_HELP_BUTTON),
                      False);

  XmListDeleteAllItems(list_id);

  /** if no base table exists just return (no data ) for list **/
  if(db_valid_db(&pscor_db) != D_OKAY)
  {
    printf("WARNING - no pscor_db recs, skip fill of cor edit selection box\n");
    return;
  }

  /** retrieve all the records (in key order) and put into list **/
  searchkey = &keyvalrecord;
  memset(&searchkey->cor.idx, '\0', sizeof(PSCoordFileRecord));
  searchkey->cor.idx = (long)0;
  strncpy(searchkey->cor.key, PSKEY_BUS, sizeof(searchkey->cor.key));

  /* stat = db_search(&pscor_db, &searchkey, &nextptr, COORD_IDX); */
  stat = db_first(&pscor_db, &nextptr, COORD_IDX);

  if(stat == D_EOF)
  {
    printf("INFORMATION: db_search did not find any ps coord file recs\n");
   /* want to print info to error dialog as warning */
    return;
  }

  cnt = 0;
  while(stat == D_FOUND)
  {
#ifdef DEBUG
    printf("%ld %s\n", nextptr->cor.idx, nextptr->cor.key); 
#endif
    xlns[cnt++] = XmStringCreate((char *)nextptr->cor.key,
        XmSTRING_DEFAULT_CHARSET);

    stat=db_next(&pscor_db, &nextptr, &nextptr, COORD_IDX);
  }

  XtVaSetValues(list_id,
                XmNitems, xlns,
                XmNitemCount, cnt,
                NULL);

  if(cnt <= 0)
  {
    printf("WARNING - no items found to put into coordinate file\n");
  }
  XmTextShowPosition(text_id,(XmTextPosition)0);

  return;
}


#ifdef XLIST
void list_comment_strs()
/***************************************************************************\
* Purpose: for debug list all comments strs from widgets
\***************************************************************************/
{
  DB_STAT stat;
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  GraphElement *pvertex;
  char *pcmt_str;
          XmString xstr;

  stat = db_valid_db(&graph_db); /*** check for a valid db **/
  if(stat == D_INVALID_DB)
  {
    printf(" INFO: graph_db for GraphElements NON-existant!\n" );
    return;
  }

  list_nth_comment( 1 );
 
  /************ Get first COMMENT record ********************************/
  pgraphnode = &graphnode;
  pgraphnode->type = GraphTypeVertex;
  pgraphnode->class = GraphClassVertexComment;
  pgraphnode->idx = (long)0;
  stat = db_search( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);

  stat = D_FOUND;

  /***** loop through all COMMENT records **********************************/
  while(stat == D_FOUND)
  {
    printf(" graph_db: %d %d\n", pgraphnodenxt->idx, pgraphnodenxt->wid );
    if( GraphClassVertexComment != pgraphnodenxt->class )
    {
      break;
    }
      if(pgraphnodenxt->wid == NULL)
      {
        printf("Error - no comment wid for C %6.2lf%6.2lf/n", 
	MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ));
      }
      else
      {
        /* first check for FONT card that must precede this */
        pvertex = get_comment_link_by_class( pgraphnodenxt,
			 GraphClassVertexFont );
        if( pvertex != NULL )
        {
          XtVaGetValues(pvertex->wid, XmNlabelString, &xstr, NULL);
          pcmt_str = cstring_to_string(xstr);
	  if( pcmt_str != NULL )
	  {
#ifdef NONO
	    printf("P %s\n", &pcmt_str[2] ); /*Change 'CP' to 'C '*/
#endif
	  }
	  else
	  {
	    printf("PROGRAM ERROR - from pswriteCoordFile (pscordat.c)\n" );
	    printf("  NULL pointer to comment widget labelString\n" );
#ifdef NONO
	    printf("C %6.2lf%6.2lf%s\n",
	    MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	    MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ),
	    "(Null)" );
#endif
	  }
	  XmStringFree(xstr);
        }

	/* SEEK out the ORIGINAL comments */
        pvertex = get_comment_link_by_class( pgraphnodenxt,
					 GraphClassVertexOrgComment );

	if(pvertex == NULL)
	{
	  printf("Error - no original wid for C %6.2lf%6.2lf\n", 
	  MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	  MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ));
	}
	else
	{
	  XtVaGetValues(pvertex->wid, XmNlabelString, &xstr, NULL);
          pcmt_str = cstring_to_string(xstr);
	  if( pcmt_str != NULL )
	  {
	    printf("    C %6.2lf%6.2lf%s\n",
	    MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	    MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ),
	    pcmt_str );

	  }
	  else
	  {
	    printf("PROGRAM ERROR - from pswriteCoordFile (pscordat.c)\n" );
	    printf("  NULL pointer to comment widget labelString\n" );
/*	    printf("C %6.2lf%6.2lf%s\n",
	    MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	    MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ),
	    "(Null)" ); */
	  }
	  /* XmStringFree(xstr);  Don't do this!  It kills the gadget str */
        }
      }

    /** Get next record **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
  }

/* debug 
  pgraphnode->type = GraphTypeVertex;
  pgraphnode->class = GraphClassVertexComment;
  pgraphnode->idx = (long)0;
  stat = db_search( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);

  stat = D_FOUND;

	    XtVaGetValues(pvertex->wid, XmNlabelString, &xstr, NULL);
            pcmt_str = cstring_to_string(xstr);
	    printf("C %6.2lf%6.2lf%s\n",
	    MOTIF_to_ps_x( pgraphnodenxt->x, "COMMENT", pcmt_str, " " ),
	    MOTIF_to_ps_y( pgraphnodenxt->y, "COMMENT", pcmt_str, " " ),
	    pcmt_str ); */
}

void list_nth_comment( int n )
{
  DB_STAT stat;
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  GraphElement *pvertex;
  char *pcmt_str;
          XmString xstr;
  int i = 0;

  /************ Get first COMMENT record ********************************/
  pgraphnode = &graphnode;
  pgraphnode->type = GraphTypeVertex;
  pgraphnode->class = GraphClassVertexComment;
  pgraphnode->idx = (long)0;
  stat = db_search( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);

  stat = D_FOUND;

  while(stat == D_FOUND && n>i++ )
  {
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
  }

  printf(" comment #%d %d %d\n", i, pgraphnodenxt->idx, pgraphnodenxt->wid );

}
#endif


void change_bus_icon_display_status( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs)
/**************************************************************************\
* Purpose: Answers the show/hide radio buttons.
*	   Set the data flags.
*	   Change the bus icon.
*	   Redisplay the bus data.
*
* Author: Bill Rogers 	2/15/1996
*
* Called by: pushing show_bus or hide_bus radio buttons on
*	     modify_bus_coord_dia dialog.
\**************************************************************************/
{
  char outbuf_array[BUFSIZE];
  char inbuf_array [BUFSIZE];
  char *pfcmdstr = outbuf_array;
  char *pfdataret = inbuf_array;
  char newkey[256];

  PSCoordFileRecord *coordrec_p;        /* allocate 1 space for ptr  */
  GraphElement graphnode;		/* work area for update 	*/
  GraphElement *pgraphnode;

  char *pctag;
  char *namebase;
  char hide_flag;
  GraphElement *pe;
  DB_STAT stat;         /* status returned for general db access        */


  pctag = (char *)tag;
  hide_flag = pctag[0];

  getCurBus( &namebase );
  coordrec_p = coord_db_bus_search( namebase );
  stat = findBusGraphVertexRec   ( namebase, &pe );

  if( (int)XmToggleButtonGetState( widget_id( "hide_bus" ) ) )
  {
    /*coordrec_p->bus.display[0] = HIDE;*/
    pe->hide = HIDE;
  }
  else
  {
    /*coordrec_p->bus.display[0] = NO_HIDE;*/
    pe->hide = NO_HIDE;
  }

  pgraphnode = &graphnode;
  memcpy(pgraphnode, pe, sizeof(GraphElement));
  stat = db_update(&graph_db, &pe, &pgraphnode);
  if(D_OKAY != stat)
  {
    printf("ERROR: from change_bus_icon_display_status (pscordat.c)\n" );
    printf("       graph db not updated with new hide flag (%s)\n",
	           pe->vertex_id);
  }
 
  /* get the bus data, and refresh  */
  sprintf(pfcmdstr, "/get_data,type=output\n");
  sprintf(newkey, "B     %12.12s   \n", pe->vertex_id);
  strcat(pfcmdstr, newkey);
  strcat(pfcmdstr, ENDOFMSG);

  ipc_synch_rw(pfdataret, pfcmdstr);	/* ask powerflow for voltage data */

  process_update_data( pfdataret );

}

void change_gen_icon_display_status( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs)
/*************************************************************************\
* Purpose: Answers the gen show/hide radio buttons.
*	   Set the data flags.
*
* Author: Bill Rogers 	4/4/1998
*
* Called by: pushing gen show_bus or hide_bus radio buttons on
*	     modify_bus_coord_dia dialog.
\**************************************************************************/
{
  char *namebase;

  GraphElement *pe;
  GraphElement *pgen;
  GraphElement gen;
  GraphElement *pnew = &gen;
  DB_STAT stat;         /* status returned for general db access  */

  getCurBus( &namebase );
  stat = findBusGraphVertexRec   ( namebase, &pe );
  findVertxLinkedToVertx   ( &pgen, pe, GraphClassVertexGenerator );

  if( pgen )
  {
  memcpy( pnew, pgen, sizeof(GraphElement));

  if( (int)XmToggleButtonGetState( widget_id( "hide_gen" ) ) )
    pnew->hide = HIDE;
  else
    pnew->hide = NO_HIDE;

  stat = db_update(&graph_db, &pgen, &pnew);
  if(D_OKAY != stat)
  {
    printf("ERROR: from createGraphGenReac (graphdata.c)\n" );
    printf("       graph db not updated with new hide flag (%s)\n",
                   pe->vertex_id);
  }
  } 
}

void change_reac_icon_display_status( Widget w, XtPointer tag,
					 XmAnyCallbackStruct *cbs)
/*************************************************************************\
* Purpose: Answers the reac show/hide radio buttons.
*	   Set the data flags.
*
* Author: Bill Rogers 	4/4/1998
*
* Called by: pushing reac show_bus or hide_bus radio buttons on
*	     modify_bus_coord_dia dialog.
\**************************************************************************/
{
  char *namebase;

  GraphElement *pe;
  GraphElement *pgen;
  GraphElement gen;
  GraphElement *pnew = &gen;
  DB_STAT stat;         /* status returned for general db access  */

  getCurBus( &namebase );
  stat = findBusGraphVertexRec   ( namebase, &pe );
  findVertxLinkedToVertx   ( &pgen, pe, GraphClassVertexReactor );

  if( pgen )
  {
  memcpy( pnew, pgen, sizeof(GraphElement));

  if( (int)XmToggleButtonGetState( widget_id( "hide_reac" ) ) )
    pnew->hide = HIDE;
  else
    pnew->hide = NO_HIDE;

  stat = db_update(&graph_db, &pgen, &pnew);
  if(D_OKAY != stat)
  {
    printf("ERROR: from createGraphGenReac (graphdata.c)\n" );
    printf("       graph db not updated with new hide flag (%s)\n",
                   pe->vertex_id);
  }
  } 
}


#ifdef EXPMTAL
void  write_out_draw_cards  ( FILE *fptr )
/***************************************************************************\
* Purpose: find and write out the draw lines in some manageable format
*
*
\***************************************************************************/
{
  GraphElement graphnode;
  GraphElement *pgraphnode = &graphnode;
  GraphElement *pgraphnodenxt;
  GraphLink *lnk;
  GraphElement *bends[MAXBENDS];
  GraphElement *pedge;
  int bendcnt, nlinks;
  GraphElement *endstarts[50];
  PSCoordFileRecord drwrec;
  PSCoordFileRecord *pdrwrec = &drwrec;
 
  DB_STAT  stat;
  int nstarts=0;
  char pen;

  printf("Routine experimental\n" );

  stat = db_valid_db(&graph_db); /*** check for a valid db **/
  if(stat != D_INVALID_DB)
  {	 
    /************ Get first DRAW vertex ********************************/
    pgraphnode = &graphnode;
    pgraphnode->type = GraphTypeVertex;
    pgraphnode->class = GraphClassVertexDrawPoint;
    pgraphnode->idx = (long)0;
    stat = db_search( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
  }

  stat = D_FOUND;

  /***** loop through all DRAW records **********************************/
  while(stat == D_FOUND)
  {
    if( GraphClassVertexDrawPoint != pgraphnodenxt->class  )
    {
      break;
    }
      if(pgraphnodenxt->wid == NULL)
      {
        fprintf(stderr, "Error - no draw wid /n" );
      }
      else
      {

	  /* try to start at an endpt - not in the middle */
	stat = ll_first( &graph_db, &pgraphnodenxt, &lnk, GRAPH_EDGE_LINK );
	nlinks = 0;
	while( stat == D_FOUND )
	{
	  nlinks++;
	  pedge = lnk->graphnode;	/* save for 1st edge start later */
	  stat = ll_next( &lnk, &lnk );
	}
	printf(" %d links to this drawpoint \n", nlinks );

/*********************** begin the walk - if... ***************************/
	if( nlinks == 1 )
	{
	  int n;
	  Boolean skip=FALSE;
		/* don't use vertex that has already been recorded */
	  if( nstarts>=1 )
	  {
	    /* loop to check if this vertex was already processed */
	    for( n=0; n<= nstarts; n++ )
 	    {
	      if( endstarts[n] == pgraphnodenxt )
		continue;
	      skip = TRUE;
	    }
	  }

	  if( skip != TRUE )
	  {			 /* definitely - start the walk */
	    int i;

	    endstarts[++nstarts] = pgraphnodenxt;	/* record so it is
							*  not used again */
	    for( i=0; i<MAXBENDS; i++)
	    {
	      bends[i] = NULL;
	    }
	    bends[1] = pgraphnodenxt;

	    bendcnt = walk_the_line( bends, GraphClassVertexDrawPoint );

	    endstarts[++nstarts] = bends[bendcnt];	/* record end pt */

	    strcpy( pdrwrec->draw.key, "D " );
	    while( n< bendcnt )
	    {
	      if( n == 0 )
	        pen = ' ';	/* move with pen up */
	      else
		pen = '1';	/* move with pen down */

	      pdrwrec->draw.pt[n].up_dn = pen;
	      sprintf( pdrwrec->draw.pt[n].x, "%f6.2", bends[n]->x );
	      sprintf( pdrwrec->draw.pt[n].y, "%f6.2", bends[n]->y );
	    }
	    fprintf( fptr, "%s", pdrwrec );
	  }
	}
 
      }

    /** Try next start point **/
    pgraphnode = pgraphnodenxt;
    stat = db_next ( &graph_db, &pgraphnode, &pgraphnodenxt, GRAPH_CLASS_IDX);
  }


}


int walk_the_line( GraphElement *bends[], int end_type )
/**************************************************************************\
* Purpose: starts at a vertex, "walks" down the edges until the end is
*	   reached	bends[0] is considered the starting pt.
*
*
\**************************************************************************/
{
  GraphElement *pev1, *pev2;
  GraphElement *last_edge;
  GraphElement *last_vtx;
  GraphElement *llretrec;
  GraphElement *nxtbend;
  int bendcnt = 1;
  DB_STAT stat;
  Boolean not_end = TRUE;

  last_vtx = bends[bendcnt];
  if( last_vtx == NULL ) printf("BAD START PARAM!\n");
  if( last_vtx->class != GraphClassVertexDrawPoint ) printf("BAD CLASS!\n");

  stat = ll_first ( &graph_db, &last_vtx, &llretrec, GRAPH_EDGE_LINK);
  if(stat != D_FOUND)
  {
    printf("  ERROR: no edge links \n");
    return (0);
  }

  while ( not_end )
  {
    stat = findEdgeVertexes(llretrec, &pev1, &pev2);
    if(D_FOUND != stat)
    {
      printf("ERROR bad vertexes found for edge\n");
      printGraphElement(bends[1]);
      return(0);
    }

    if( pev1 == last_vtx )
      nxtbend = pev2;
    else if( pev2 == last_vtx )
      nxtbend = pev1;
    else
     printf("ERROR - no vertex matches\n");
 
    bends[++bendcnt] = nxtbend;

    if( nxtbend->class == end_type )
	not_end = FALSE;

    last_edge = llretrec;

    stat = ll_first( &graph_db, &nxtbend, &llretrec, GRAPH_EDGE_LINK);
    if(stat != D_FOUND)
    {
      printf("  ERROR: no edge links \n");
      return (0);
    }
    if ( last_edge == llretrec )	/* get the other one */
    {
      stat = ll_next( &llretrec, &llretrec );
      if(stat != D_FOUND)
      {
        printf("  ERROR: no edge links \n");
        return (bendcnt);
      }
    }
  }

  return (bendcnt);
}
#endif
