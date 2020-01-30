/*									    */
/* Include file from VUIT						    */
/*	1. VUIT global routine declarations				    */
/*	2. Global variables						    */
/*	3. Literals from UIL code					    */

/* VUIT routines for the user to call */
void s_error();
int HashFunction();
int HashLookup();
int HashRegister();
void VUIT_Manage();
void VUIT_Unmanage();

/* Motif Global variables */
Display         *display;		/* Display variable */
XtAppContext    app_context;		/* application context */
Widget		toplevel_widget;	/* Root widget ID of application */
MrmHierarchy	s_MrmHierarchy;		/* MRM database hierarchy ID */

/* Literals from the UIL code */

