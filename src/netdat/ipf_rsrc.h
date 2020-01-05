/* static char sccsid[] = "@(#)ipf_rsrc.h	20.2 12/22/94" */
/**************************************************************
* file: ipf_rsrc.h
* application:  interactive powerflow
* purpose: define the applicaton resources for the x windows
* author: Dan Clark 
* note:
*    The file: ipf_ipc.h  must be included before this file is included.
*  - The resource table defined here is hardcoded in the applicaton
*    and when the table is specified on widget creation it 
*    overrides the resources that may have been specified by
*    the other means of defining resources:
*    (see X toolkit intrinsices pgm manual by ORielly & Assoc p. 245) 
*
*    Generally the following are loaded into the resource database
*     when the application starts up with 
*     the xtinit or xtappinit or xttoolkitinit.
*     a) resource values specified in the application-defaults file.
*       the file for this application is XGUI located in home directory
*       and should be properly named (needs same name as specified in
*       the xtinit function) and should be properly installed in the
*       directory /usr/lib/X11/app-defaults
*     b) resources specified in the .Xdefaults file in users home dir
*     c) resource manager properties stored in the server by the
*       user with xrdb (one of the recommended ways to install
*       the defaults in the app-defaults file)
*     d) command line options 
*
**************************************************************
*/

/*** define the pair of constaints name & class of the resource ***/
/*** only define those not already included in the stringdefs.h ***/

#define XgNerrmsg "errmsg"
#define XgCErrmsg "Errmsg" 
#define XgDefaultErrmsg False 

#define XgNserver "server"
#define XgCServer "Server" 
#define XgDefaultServer True 

#define XgNnode "node"
#define XgCNode "Node" 
#define XgDefaultNode NULL
/* #define XgDefaultNode "nodename" */

#define XgNservername "servername"
#define XgCServername "Servername"
#define XgDefaultServername "ipfsrv" 

#define XgNdebug "debug"
#define XgCDebug "Debug" 
#define XgDefaultDebug 0

#define XgNsocketid "socketid"
#define XgCSocketid "Socketid" 
#define XgDefaultSocketid I_SOCKID

#define XgNcasename "casename"
#define XgCCasename "Casename"
#define XgDefaultCasename "casename" 

#define XgNsleep "sleep"
#define XgCSleep "Sleep" 
#define XgDefaultSleep 0 

#define XgNautostart "autostart"
#define XgCAutostart "Autostart" 
#define XgDefaultAutostart False

#define XgNgraphorder "graphorder"
#define XgCGraphorder "Graphorder" 
#define XgDefaultGraphorder 0

/*** application data structure for variables set via resources ***/
typedef struct {
    Boolean	errmsg;
    int		debug;
    Boolean	server;
    int		socketid;
    char*	servername;
    char*	casename;
    char*	node;
    int		sleep;
    Boolean	autostart;
    Boolean	graphorder;
} AppResData, *AppResDataPtr;

/* static AppResData app_resdata; */

/*** define the resource list entries ***/

static XtResource resources[] = {
    {
	XgNerrmsg,
	XgCErrmsg,
	XtRBoolean,
	sizeof(Boolean),
	XtOffset(AppResDataPtr, errmsg),
	XtRImmediate,
	(XtPointer) XgDefaultErrmsg,
    },
    {
	XgNdebug,
	XgCDebug,
	XtRInt,
	sizeof(int),
	XtOffset(AppResDataPtr, debug),
	XtRImmediate,
	(XtPointer) XgDefaultDebug,
    },
    {
	XgNservername,
	XgCServername,
	XtRString,
	sizeof(char*),
	XtOffset(AppResDataPtr, servername),
	XtRString,
	(XtPointer) XgDefaultServername,
    },
    {
	XgNserver,
	XgCServer,
	XtRBoolean,
	sizeof(Boolean),
	XtOffset(AppResDataPtr, server),
	XtRImmediate,
	(XtPointer) XgDefaultServer,
    },
    {
	XgNsocketid,
	XgCSocketid,
	XtRInt,
	sizeof(int),
	XtOffset(AppResDataPtr, socketid),
	XtRImmediate,
	(XtPointer) XgDefaultSocketid,
    },
    {
	XgNcasename,
	XgCCasename,
	XtRString,
	sizeof(char*),
	XtOffset(AppResDataPtr, casename),
	XtRString,
	(XtPointer) XgDefaultCasename,
    },
    {
	XgNnode,
	XgCNode,
	XtRString,
	sizeof(char*),
	XtOffset(AppResDataPtr, node),
	XtRString,
	(XtPointer) XgDefaultNode,
    },
    {
	XgNsleep,
	XgCSleep,
	XtRInt,
	sizeof(int),
	XtOffset(AppResDataPtr, sleep),
	XtRImmediate,
	(XtPointer) XgDefaultSleep,
    },
    {
	XgNautostart,
	XgCAutostart,
	XtRBoolean,
	sizeof(Boolean),
	XtOffset(AppResDataPtr, autostart),
	XtRImmediate,
	(XtPointer) XgDefaultAutostart,
    },
    {
	XgNgraphorder,
	XgCGraphorder,
	XtRBoolean,
	sizeof(Boolean),
	XtOffset(AppResDataPtr, graphorder),
	XtRImmediate,
	(XtPointer) XgDefaultGraphorder,
    },
}; /* end resource list */

/* command line options table */
/****
* accepting resources from the command line requires the
* application to have a c defined structure of the custom
* command line options (beyond the standard set) and
* passing this options table as an argument to xtinit function
****/

static XrmOptionDescRec options[] = {
    {"-errmsg",     "*errmsg",      XrmoptionNoArg,         "False"},
    {"+errmsg",     "*errmsg",      XrmoptionNoArg,         "True"},
    {"-debug",      "*debug",       XrmoptionSepArg,         0},
    {"-servername", "*servername",  XrmoptionSepArg,        NULL},
    {"-server",     "*server",      XrmoptionNoArg,         "False"},
    {"+server",     "*server",      XrmoptionNoArg,         "True"},
    {"-socketid",   "*socketid",    XrmoptionSepArg,         0},
    {"-casename",   "*casename",    XrmoptionSepArg,        NULL},
    {"-node",       "*node",        XrmoptionSepArg,        NULL},
    {"-sleep",      "*sleep",       XrmoptionSepArg,         0},
    {"-autostart",  "*autostart",   XrmoptionNoArg,         "False"},
    {"+autostart",  "*autostart",   XrmoptionNoArg,         "True"},
    {"-graphorder",  "*graphorder",   XrmoptionNoArg,         "False"},
    {"+graphorder",  "*graphorder",   XrmoptionNoArg,         "True"},
};

