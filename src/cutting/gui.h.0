/*
*                      Interactive PowerFlow (IPF)
*               Bonneville Power Administration, Portland, OR.
*
*         Authors:  D. L. Clark,     J. G. Coleman,     W. L. Powell,
*                   B. E. Rogers,    K. E. Rowell,      J. L. Rutis,
*                   D. M. Stefonek,  D. B. Szymanski,   T.    Liu
*
*
*                              NOTICE
*
* Interactive Powerflow (IPF) was developed by BPA and its contractors
* with about 20% of the cost supported by the Electric Power Research
* Institute (EPRI).  By mutual agreement, as described in EPRI Agreement
* RP2746-03 entitled Graphical User Interface for Powerflow, March, 1992,
* all results of this project--including the computer program and its
* documentation--are to be in the public domain.  In a separate Memorandum
* of Understanding with the Western Systems Coordinating Council (WSCC),
* BPA agreed in March, 1992, to keep WSCC informed of progress, to make
* its best effort to develop the program according to the Guidelines
* adopted by the WSCC Computer Program Management Subcommittee, and to
* make the final results available for possible further development by
* WSCC. 
*
* This notice must appear in all copies of this software and documentation.
*
* This notice shall not be removed from any module. 
*
* 1. There is no charge for program development costs, however, a fee
* covering costs incurred in responding to requests is charged to the
* organization receiving materials non-electronically.  This fee also 
* covers resources, reproduction, media preparation, and shipping.
*
* 2. The BPA does not provide assistance with the conversion of its
* programs to other computer systems.
*
* 3. The BPA does not provide consulting services to its users.
*
* 4. In consideration and receipt or acceptance of programs and related
* documentation, you and your orgranization agree to advise any
* third-party receipients (in writing) that the program(s) and/or
* documetation are in the public domain and available from the BPA in the
* event thay are sold, assigned, or transferered to other orgainzations.
* This process ensures that BPA-developed programs be identified as such
* to recipients.
* 
* Interactive PowerFlow Program (IPF)
* The Interactive PowerFlow Program models the balanced steady-state
* operation of an electric power network.  It is used by power system
* planners and design engineers to investigate electric power networks,
* determine bus voltage distribution, line real and reactive power flows,
* line overloads, system reactive reaquirements, area controls, effect of
* load shedding, generator, dropping, and line outages.
* 
* Currently, the program efficiently uses computer resources through
* the implementation of advanced techniques of large system analyses,
* including the Newton-Raphson method for solving second order algebraic
* equations and sparse-matrix computation techniques.  Typically, it
* solves a 6000 bus network and requires 20 MB of virtual storage.  The
* state-of-the-art Graphical User Interface (GUI) has been integrated into
* the IPF using the X Window System/Motif.
*
* The IPF runs on various platforms which support the X Window
* System/Motif.  At BPA, IPF is used on VAX VMS workstations,  
* DEC Alpha workstations running either OPEN VMS or OSF UNIX,
* and properly configured PCs, typically running the UNIX operating system.
*
* Prepared by:
* Planning Methods Section
* Systems Engineering Branch
* Bonneville Power Administration
* P.O. Box 3621 -- EOHB
* Portland, OR  97208
* May 1994
*
*/
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
Widget VUIT_Manage();
void VUIT_Unmanage();

/* Motif Global variables */
Display         *display;		/* Display variable */
XtAppContext    app_context;		/* application context */
Widget		toplevel_widget;	/* Root widget ID of application */
MrmHierarchy	s_MrmHierarchy;		/* MRM database hierarchy ID */

/* Literals from the UIL code */

