static char sccsid[]="@(#)filetest.c	20.2 5/16/94";
#include <stdio.h>                      /* For printf and so on. */
#include <Xm/Xm.h>

/* The vuit generated application include file.				    */
/* If the user does not specify a directory for the file		    */
/* then the vaxc$include logical needs to be defined to point to the	    */
/* directory of the include file.					    */
#include "file_test.h"

/*
 * Global data
 */
static MrmType class_id;		/* Place to keep class ID*/
static MrmType *dummy_class;            /* and class variable. */
static char *db_filename_vec[] =        /* Mrm.hierachy file list. */
  {
"file_test.uid"
  };
static int db_filename_num =
                (sizeof db_filename_vec / sizeof db_filename_vec [0]);
char *vuit_dummy_ident_value = "VUIT dummy identifier value";
int i;
#define hash_table_limit 500
struct HASH_TABLE_STRUCT
    {
    char	*widget_name;
    Widget	id;
    } hash_table[hash_table_limit + 1];

/*
 * Forward declarations
 */
void file_exit_cb();
void file_open_cb();
void file_open_ok_cb();
void file_open_cancel_cb();
void file_save_cb();
void file_save_ok_cb();
void file_saveas_cancel_cb();
void file_print_cb();
void file_open_base_cb();
void file_openbase_ok_cb();
void file_openbase_cancel_cb();
void file_printbase_cb();
void file_openbase_cb();
void file_printgraph_cb();
void build_graph_cb();
void file_connectgraph_cb();
void file_display_graph_cb();
void file_display_graph_edges_cb();
void register_name_cb();

/*
 * Names and addresses of callback routines to register with Mrm
 */
static MrmRegisterArg reglist [] = {
{"file_exit_cb", (caddr_t)file_exit_cb},
{"file_open_cb", (caddr_t)file_open_cb},
{"file_open_ok_cb", (caddr_t)file_open_ok_cb},
{"file_open_cancel_cb", (caddr_t)file_open_cancel_cb},
{"file_save_cb", (caddr_t)file_save_cb},
{"file_save_ok_cb", (caddr_t)file_save_ok_cb},
{"file_saveas_cancel_cb", (caddr_t)file_saveas_cancel_cb},
{"file_print_cb", (caddr_t)file_print_cb},
{"file_open_base_cb", (caddr_t)file_open_base_cb},
{"file_openbase_ok_cb", (caddr_t)file_openbase_ok_cb},
{"file_openbase_cancel_cb", (caddr_t)file_openbase_cancel_cb},
{"file_printbase_cb", (caddr_t)file_printbase_cb},
{"file_openbase_cb", (caddr_t)file_openbase_cb},
{"file_printgraph_cb", (caddr_t)file_printgraph_cb},
{"build_graph_cb", (caddr_t)build_graph_cb},
{"file_connectgraph_cb", (caddr_t)file_connectgraph_cb},
{"file_display_graph_cb", (caddr_t)file_display_graph_cb},
{"file_display_graph_edges_cb", (caddr_t)file_display_graph_edges_cb},
{"register_name_cb", (caddr_t)register_name_cb}};

static int reglist_num = (sizeof reglist / sizeof reglist[0]);

/*
 * Names and addresses of uil identifiers (if any) to register with Mrm.
 * These identifiers are registered with a dummy value to allow the generated 
 * code to run without error.
 * You can avoid the registration of these identifiers by simplying editing
 * this template file (vuit_main_template_c) and removing the following
 * special format comments:
 *	***VUIT ident registration***
 *	***VUIT identlist size***
 *	***VUIT register identifiers***
 * You can provide your own registration of identifiers by calling your own
 * routine in response to a callback (such as the MrmNcreateCallback for your
 * application's main window), or by modifying this template to call your
 * own registration routine.
 */



/*
 * OS transfer point.  The main routine does all the one-time setup and
 * then calls XtAppMainLoop.
 */
unsigned int main(argc, argv)
    unsigned int argc;                  /* Command line argument count. */
    char *argv[];                       /* Pointers to command line args. */
{
    Arg arglist[2];
    int n;

    MrmInitialize();			/* Initialize MRM before initializing */
                                        /* the X Toolkit. */
    DXmInitialize();			/* Initialize additional DEC widgets */

    /* 
     * If we had user-defined widgets, we would register them with Mrm.here. 
     */

    /* 
     * Initialize the X Toolkit. We get back a top level shell widget.
     */
    XtToolkitInitialize();

    app_context = XtCreateApplicationContext();
    display = XtOpenDisplay(app_context, NULL, argv[0], "example",
                            NULL, 0, &argc, argv);
    if (display == NULL) 
	{
        fprintf(stderr, "%s:  Can't open display\n", argv[0]);
        exit(1);
	}

    n = 0;
    XtSetArg(arglist[n], XmNallowShellResize, True);  n++;
    toplevel_widget = XtAppCreateShell(argv[0], NULL, applicationShellWidgetClass,
                              display, arglist, n);

    /* 
     * Open the UID files (the output of the UIL compiler) in the hierarchy
     */
    if (MrmOpenHierarchy(db_filename_num, /* Number of files. */
      db_filename_vec,                    /* Array of file names.  */
      NULL,                               /* Default OS extenstion. */
      &s_MrmHierarchy)                    /* Pointer to returned MRM ID */
      !=MrmSUCCESS)
        s_error("can't open hierarchy");

MrmRegisterNames (reglist, reglist_num);


VUIT_Manage("file_test");

    /* 
     * Realize the top level widget.  All managed children now become visible
     */
    XtRealizeWidget(toplevel_widget);

    /* 
     * Sit around forever waiting to process X-events.  We never leave
     * XtAppMainLoop. From here on, we only execute our callback routines. 
     */
    XtAppMainLoop(app_context);
}


/*
 * All errors are fatal.
 */
void s_error(problem_string)
    char *problem_string;
{
    printf("%s\n", problem_string);
    exit(0);
}

void VUIT_Manage(widget_name)
    char	*widget_name;
{
    Widget		id;
    Window		pop_window;
    XWindowChanges	values;

    if (HashLookup(widget_name, &id))
	if (XtIsManaged(id))
	    {
	    pop_window = XtWindow(XtParent(id));
	    values.x = values.y = values.width = values.height =
		values.border_width = values.sibling = NULL;
	    values.stack_mode = Above;
	    XConfigureWindow(display, pop_window, CWStackMode, &values);
	    }
	else
	    XtManageChild(id);
    else
	{
	MrmFetchWidget(s_MrmHierarchy, widget_name, toplevel_widget, &id, 
	    &class_id);
	XtManageChild(id);
	HashRegister(widget_name, id);
	}
}

void VUIT_Unmanage(widget_name)
    char	*widget_name;
{
    Widget	id;

    if (HashLookup(widget_name, &id))
	XtUnmanageChild(id);
}

int HashRegister (widget_name, id)
    char		*widget_name;
    Widget		id;    
{
    int			ndx;

    for (ndx = HashFunction(widget_name, hash_table_limit);
	((hash_table[ndx].widget_name != NULL) &&
	    (ndx < hash_table_limit));
	ndx++);
    if (hash_table[ndx].widget_name != NULL)
	for (ndx = 0;
	    hash_table[ndx].widget_name != NULL;
	    ndx++);
    if (ndx > hash_table_limit)
	return (FALSE);
    else
	{
	hash_table[ndx].widget_name = XtCalloc(1, strlen(widget_name) + 1);
	strcpy(hash_table[ndx].widget_name, widget_name);
	hash_table[ndx].id = id;
	return (TRUE);
	}
}


int HashLookup (name, id)
    char		*name;
    Widget		*id;
{
    int			ndx;

    for (ndx = HashFunction(name, hash_table_limit);
	((hash_table[ndx].widget_name != NULL) &&
	    (ndx <= hash_table_limit));
	ndx++)
	if (strcmp(name, hash_table[ndx].widget_name) == 0)
	    {
	    *id = hash_table[ndx].id;
	    return (TRUE);
	    }

    if (ndx > hash_table_limit)
	for (ndx = 0;
	    ((hash_table[ndx].widget_name != NULL) &&
		(ndx <= hash_table_limit));
	    ndx++)
	    {
	    if (strcmp(name, hash_table[ndx].widget_name) == 0)
		{
	 	*id = hash_table[ndx].id;
		return (TRUE);
		}
	    }

    return (FALSE);
}

int HashFunction (name, max)
    char		*name;
    int			max;

{
#define HashVecSize		20	/* plenty for 31 character names */
typedef union
    {
    short int		intname[HashVecSize];	 /* name as vector of ints */
    char		charname[2*HashVecSize]; /* name as vector of chars */
    } HashName;

    HashName		locname;	/* aligned name */
    int			namelen;	/* length of name */
    int			namelim;	/* length limit (fullword size) */
    int			namextra;	/* limit factor remainder */
    int			code = 0;	/* hash code value */
    int			ndx;		/* loop index */


    /*
     * Copy the name into the local aligned union.
     * Process the name as a vector of integers, with some remaining characters.
     * The string is copied into a local union in order to force correct
     * alignment for alignment-sensitive processors.
     */
    strcpy (locname.charname, name);
    namelen = strlen (locname.charname);
    namelim = namelen >> 1;		/* divide by 2 */
    namextra = namelen & 1;		/* remainder */

    /*
     * XOR each integer part of the name together, followed by the trailing
     * 0/1 character
     */
    for ( ndx=0 ; ndx<namelim ; ndx++ )
        code = code ^ ((locname.intname[ndx])<<ndx);
    if ( namextra > 0 )
        code = code ^ ((locname.intname[ndx])&0x00FF);

    return (code&0x7FFF) % max;
}


void file_exit_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
exit(0);
}

void file_open_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("file_select_dialog");
}

void file_open_ok_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
readCoordData(w, tag, reason);
VUIT_Unmanage("file_select_dialog");
}

void file_open_cancel_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("file_select_dialog");
}

void file_save_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("file_saveas_dialog");
}

void file_save_ok_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
writeCoordFile(w, tag, reason);
VUIT_Unmanage("file_saveas_dialog");
}

void file_saveas_cancel_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Unmanage("file_saveas_dialog");
}

void file_print_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
printCoordData(w, tag, reason);
}

void file_open_base_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("file_open_base");
}

void file_openbase_ok_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
readBaseData(w, tag, reason);
VUIT_Unmanage("file_openbase_dialog");
}

void file_openbase_cancel_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("file_openbase_dialog");
}

void file_printbase_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
printBaseData(w, tag, reason);
}

void file_openbase_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
VUIT_Manage("file_openbase_dialog");
}

void file_printgraph_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
printGraphData(w, tag, reason);
}

void build_graph_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
creategraphtbl(w, tag, reason);
buildGraphCoord(w, tag, reason);
buildGraphBase(w, tag, reason);
}

void file_connectgraph_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
connectGraphData(w, tag, reason);
}

void file_display_graph_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
createVertexGadgets(w, tag, reason);
}

void file_display_graph_edges_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
drawGraphEdges(w, tag, reason);
}

void register_name_cb (w, tag, reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
HashRegister(XtName(w), w);
}

